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

#[derive(PartialEq)]
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
            if diff.ratio() != 1.0 {
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

    // run returns Result so that we can gracefully bubble up terminal errors
    let res = run(&mut term, &mut app);

    // ── restore the terminal before exiting ──────────────────────────────────
    disable_raw_mode()?;
    execute!(term.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
    term.show_cursor()?;

    res?;
    Ok(())
}

fn run<B: Backend>(term: &mut Terminal<B>, app: &mut App) -> io::Result<()> {
    loop {
        // draw current counters
        term.draw(|f| ui(f, app))?;

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
fn ui(f: &mut Frame<>, app: &App) {
    let outer = Block::default()
        .title("Fuzz-run stats  (press q to quit)")
        .borders(Borders::ALL);
    let area  = f.size();
    let inner = outer.inner(area);

    f.render_widget(outer, area);

    let text = vec![
        Line::from(format!("runs        : {}", app.num_runs)),
        Line::from(format!("failures    : {}", app.num_failures)),
        Line::from(format!("un-compiles : {}", app.num_uncompiles)),
        Line::from(format!("hangs       : {}", app.num_hangs)),
    ];
    f.render_widget(Paragraph::new(text), inner);
}

/// Stub that mutates the counters.
/// Swap this out for *your* loop body (the `trial(&cli, &configuration)` logic).
fn step(app: &mut App) {
    // ------------------------------------------------------------------------
    // REPLACE everything in this function with your real code:
    //
    //    let result = trial(&cli, &configuration).unwrap();
    //    if !result.we_terminate && !result.they_terminate { app.num_hangs += 1; }
    //    if !result.compiles                               { app.num_uncompiles += 1; }
    //    if !result.good()                                { app.num_failures += 1; }
    //    app.num_runs += 1;
    //
    // ------------------------------------------------------------------------

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

        let mut test_case_path = cli.out.clone();
        test_case_path.push(format!("{}.cl", now));

        fs::write(test_case_path, &result.test_case).unwrap();

        app.num_failures += 1
    }
    app.num_runs += 1;


    // use rand::{thread_rng, Rng};
    // let mut rng = thread_rng();
    // let roll: u8 = rng.gen_range(0..=100);

    // if roll <  5 { app.num_failures   += 1; } // ~5 %
    // if roll > 90 { app.num_uncompiles += 1; } // ~9 %
    // if roll == 42 { app.num_hangs     += 1; } // Easter-egg
    // app.num_runs += 1;
}
