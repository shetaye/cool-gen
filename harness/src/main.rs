use std::{
    error::Error,
    io,
    time::{Duration},
};

use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{
        disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
    },
};
use ratatui::{
    prelude::*,
    widgets::*,
};

use cool_gen::{generate_with_config, ProgramMutationConfig};
use std::{time::{SystemTime, UNIX_EPOCH}};
use similar::{TextDiff};
use std::{path::PathBuf};
use anyhow::Result;
use std::fs;

use clap::Parser;
use toml;

use short_uuid::ShortUuid;
use std::io::Write;

mod run;
use crate::run::{run as run_program};

#[derive(Parser, Debug)]
struct Args {
    /// Path to target script under test
    target: PathBuf,
    /// Path to gold script under test
    gold: PathBuf,
    /// Output path
    out: PathBuf,

    /// Path to .toml configuration for mutator
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    /// Number of mutation runs to do, defaults to 100
    #[arg(short, long)]
    num_trials: Option<usize>,
    /// How long to wait before timing out the run
    #[arg(short, long)]
    timeout: Option<u64>,
}

#[derive(PartialEq, Debug)]
enum Outcome {
    Match,
    Mismatch
}

struct TrialResult {
    /// did our program output match?
    outcome: Outcome,
    /// did we terminate?
    we_terminate: bool,
    /// did they terminate?
    they_terminate: bool,
    /// did the generated code compile?
    compiles: bool,
    /// the test case
    test_case: String
}

impl TrialResult {
    /// what is our criterion for good
    fn good(&self) -> bool {
        return 
            self.outcome == Outcome::Match ||
            !self.we_terminate && !self.they_terminate ||
            !self.compiles
            
    }
}

fn trial(cli: &Args, configuration: &ProgramMutationConfig) -> Result<TrialResult> {
    // mutate!
    let program = generate_with_config(*configuration);
    // run them!
    let them = run_program(
        cli.gold.to_str().expect("Can't coerce gold script path into string."),
        &program,
        cli.timeout
    )?;
    let us = run_program(
        cli.target.to_str().expect("Can't coerce target script path into string."),
        &program,
        cli.timeout
    )?;
    // diff them!
    let diff = TextDiff::from_lines(
        &us.output,
        &them.output
    );
    // 1.0 => complete match; anything else, we have to then return something
    Ok(
        TrialResult {
            outcome: 
            if diff.ratio() == 1.0 {
                Outcome::Match
            } else {
                Outcome::Mismatch
            },
            we_terminate: us.termination,
            they_terminate: them.termination,
            compiles: them.compilation,
            test_case: program
        }
    )
}

/// The counters you want to display.
struct App {
    num_uncompiles: usize,
    num_failures:   usize,
    num_hangs:      usize,
    num_runs:       usize,
    total_runs:     usize,
    cli:            Args,
    config:         ProgramMutationConfig
}

impl App {
    fn new(total_runs: usize, cli: Args, config: ProgramMutationConfig) -> Self {
        Self {
            num_uncompiles: 0,
            num_failures:   0,
            num_hangs:      0,
            num_runs:       0,
            total_runs,
            cli,
            config
        }
    }
    fn finished(&self) -> bool {
        self.num_runs >= self.total_runs
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Args::parse();

    // parse config or take default
    let configuration = if let Some(ref path) = cli.config {
        // check if path exists, otherwise panic before
        // throwing weird parsing errors
        assert!(path.exists(), "Configuration path doesn't exist!");

        toml::from_str::<ProgramMutationConfig>(
            &fs::read_to_string(path.to_str().unwrap()).unwrap()
        ).unwrap()
    } else {
        ProgramMutationConfig::default()
    };

    // Check that the output path exists
    assert!(cli.out.exists(), "Output path doesn't exist!");

    // ── parse CLI etc. ────────────────────────────────────────────────────────
    let total_trials = cli.num_trials.unwrap_or(100);

    // ── terminal initialisation ──────────────────────────────────────────────
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend  = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend)?;

    let mut app = App::new(total_trials, cli, configuration);

    // ── run the TUI loop ─────────────────────────────────────────────────────
    run(&mut term, &mut app)?;

    // ── restore the terminal ─────────────────────────────────────────────────
    disable_raw_mode()?;
    execute!(term.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
    term.show_cursor()?;

    // ── show final statistics on the *normal* screen and leave them there ───
    println!("\n Final statistics");
    println!(" =================");
    println!(" runs        : {}", app.num_runs);
    println!(" failures    : {}", app.num_failures);
    println!(" bad code    : {}", app.num_uncompiles);
    println!(" hangs       : {}", app.num_hangs);
    println!();
    std::io::stdout().flush()?;   // make sure everything is written

    Ok(())
}

fn run<B: Backend>(term: &mut Terminal<B>, app: &mut App) -> io::Result<()> {
    loop {
        // draw current counters
        term.draw(|f| ui::<B>(f, app))?;

        // allow the user to quit with `q`
        if event::poll(Duration::from_millis(10))? {
            if let Event::Key(key) = event::read()? {
                if key.code == KeyCode::Char('q') {
                    break;
                }
            }
        }

        if app.finished() {
            break;
        }

        // 1 "trial" — replace `step` with your real logic
        step(app);
    }
    Ok(())
}

/// Render one frame.

fn ui<B: Backend>(f: &mut Frame, app: &App) {
    // ── border ─────────────────────────────────────────────────────────────
    let outer = Block::bordered()
        .title(" Stats  (press q to quit) ")
        .border_type(BorderType::Rounded);


    // pick a fixed width/height for the stats panel
    let w = 36.min(f.size().width);   // 32 cols or whatever fits
    let h = 8;                    // 6 rows (title + 4 lines + border)

    // anchor it bottom-right
    let full = Rect::new(5, 5, w, h);
    let inner  = outer.inner(full); // rectangle just inside the border

    // add one extra cell of horizontal/vertical padding
    let pad = Rect {
        x: inner.x + 1,
        y: inner.y + 1,
        width:  inner.width.saturating_sub(2),
        height: inner.height.saturating_sub(2),
    };

    f.render_widget(outer, full);

    // ── text with a red “failures” counter ────────────────────────────────
    let lines = vec![
        Line::from(format!(" runs        : {}", app.num_runs)),
        Line::from(vec![
            Span::raw(" failures    : "),
            Span::styled(app.num_failures.to_string(), Style::default().fg(Color::Red)),
        ]),
        Line::from(format!(" bad code    : {}", app.num_uncompiles)),
        Line::from(format!(" hangs       : {}", app.num_hangs)),
    ];

    f.render_widget(Paragraph::new(lines), pad);
}

fn step(app: &mut App) {
    let cli = &app.cli;
    let configuration = app.config;

    // TODO are there any case where program crashes needs to be cleanly ignored?
    let result = trial(&cli, &configuration).unwrap(); 
    if !result.we_terminate && !result.they_terminate {
        app.num_hangs += 1;
    }
    if !result.compiles {
        app.num_uncompiles += 1;
    }
    if !result.good() {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
        let uid = ShortUuid::generate();

        let mut test_case_path = cli.out.clone();
        test_case_path.push(format!("{}-{}-CRASH.cl", uid, now));

        fs::write(test_case_path, &result.test_case).unwrap();

        app.num_failures += 1
    }
    app.num_runs += 1;
}
