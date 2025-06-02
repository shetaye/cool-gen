use cool_gen::{generate_with_config, ProgramMutationConfig};
use std::time::{SystemTime, UNIX_EPOCH};
use similar::{ChangeTag, TextDiff};
use std::{default, path::PathBuf};
use anyhow::Result;
use std::fs;


use clap::Parser;
use toml;

mod run;
use crate::run::run;

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
    /// Number of mutation runs to do
    #[arg(short, long)]
    num_trials: Option<usize>,
    /// How long to wait before timing out the run
    #[arg(short, long)]
    timeout: Option<u64>,
}

enum Termination {
    Match,
    Mismatch(String)
}

struct TrialResult {
    termination: Termination,
    both_ended: bool
}

fn trial(cli: &Args, configuration: &ProgramMutationConfig) -> Result<TrialResult> {
    // mutate!
    let program = generate_with_config(*configuration);
    // run them!
    let (them, they_ended) = run(
        cli.gold.to_str().expect("Can't coerce gold script path into string."),
        &program,
        cli.timeout
    )?;
    let (us, we_ended) = run(
        cli.target.to_str().expect("Can't coerce target script path into string."),
        &program,
        cli.timeout
    )?;
    // diff them!
    let diff = TextDiff::from_lines(
        &us,
        &them
    );
    // 1.0 => complete match; anything else, we have to then return something
    Ok(
        TrialResult {
            termination: 
            if diff.ratio() != 1.0 {
                Termination::Match
            } else {
                Termination::Mismatch(program)
            },
            both_ended: they_ended && we_ended
        }
    )
}

fn main() {
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

    // perform k runs
    loop {
        // Check that the output path exists
        assert!(cli.out.exists(), "Output path doesn't exist!");
        // Counter for num failures
        let mut num_failures = 0;
        let mut num_hangs = 0;

        // TODO are there any case where program crashes needs to be cleanly ignored?
        let result = trial(&cli, &configuration).unwrap(); 
        if !result.both_ended {
            num_hangs += 1;
        }
        match result.termination {
            Termination::Mismatch(prog) => {
                // for uniquely writing failures based on time
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
                // 
            },
            _ => {}
        }
    }
}
