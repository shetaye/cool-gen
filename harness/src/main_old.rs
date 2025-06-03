use cool_gen::{generate_with_config, ProgramMutationConfig};
use std::{time::{SystemTime, UNIX_EPOCH}};
use similar::{TextDiff};
use std::{path::PathBuf};
use anyhow::Result;
use std::fs;

use clap::Parser;
use toml;

mod run;
use crate::run::{run};

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
    let them = run(
        cli.gold.to_str().expect("Can't coerce gold script path into string."),
        &program,
        cli.timeout
    )?;
    let us = run(
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

    // Counter for num failures
    let mut num_uncompiles = 0;
    let mut num_failures = 0;
    let mut num_hangs = 0;
    let mut num_runs = 0;

    // perform k runs
    loop {
        // Check that the output path exists
        assert!(cli.out.exists(), "Output path doesn't exist!");

        // TODO are there any case where program crashes needs to be cleanly ignored?
        let result = trial(&cli, &configuration).unwrap(); 
        if !result.we_terminate && !result.they_terminate {
            num_hangs += 1;
        }
        if !result.compiles {
            num_uncompiles += 1;
        }
        if !result.good() {
            let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();

            let mut test_case_path = cli.out.clone();
            test_case_path.push(format!("{}.cl", now));

            fs::write(test_case_path, &result.test_case).unwrap();

            num_failures += 1
        }
        num_runs += 1;

        if num_runs > cli.num_trials.unwrap_or(100) { break; }
    }
}
