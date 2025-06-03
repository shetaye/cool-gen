use anyhow::{Result, anyhow};

use subprocess::{Popen, PopenConfig, ExitStatus, Redirection};
use tempfile::NamedTempFile;
use std::{fs, time::Duration};

/// Result of a program invocation.
pub struct RunResult {
    /// The test case.
    #[allow(dead_code)]
    pub input: String,
    /// The result.
    pub output: String,
    /// `true` if the program successfully compiled.
    pub compilation: bool,
    /// `true` if the test case terminates.
    pub termination: bool
}

/// run a script for a bounded number of seconds
///
/// # Arguments
/// - `script`: the path to the script to run thet test case against
/// - `test_case`: the *raw cool code*, not file, of the test harness
/// - `timeout`: number of seconds before we kill the script; defaults to 5
///
/// # Notes
/// We will *ignore* stderr and *return* stdout. 
///
/// # Returns
/// We return the output and True if the program terminated; otherwise
/// we return the output and False.
///
pub fn run(script: &str, test_case: &str, timeout: Option<u64>) -> Result<RunResult> {
    // We dump the tset case to a temporary file
    let file = NamedTempFile::new()?;
    fs::write(&file, test_case)?;

    // Run test case in a bounded number of seconds
    let mut p = Popen::create(
        &[script,
          file.path().to_str().ok_or(
              anyhow!("Unable to coerce temp path into string."))?],
        PopenConfig {
            stdout: Redirection::Pipe,
            ..Default::default()
        },
    )?;

    // Obtain the output from the standard streams.
    let mut communicator = p.communicate_start(None);
    communicator = communicator.limit_time(Duration::from_secs(timeout.unwrap_or(5)));
    let read = communicator.read_string();

    let output = (if let Ok((res, _)) = read {
        res
    } else if let Err(err) = read {
        err.capture.0.map(|f| {String::from_utf8(f).unwrap()})
    } else {
        unreachable!()
    }).ok_or(anyhow!("Unable to parse the output streams from process."))?;

    let (termination, compilation) = 
        if let Some(exit_status) = p.poll() {
            match exit_status {
                ExitStatus::Exited(s) => (true, s == 0),
                _ => (true, true)
            }
        } else {
            p.terminate()?;
            (false, true)
        };

    // If we are still going, no we're not
    // note that the timeout is brought by the above
    Ok(RunResult {
        input: test_case.into(),
        output: output,
        termination: termination,
        compilation: compilation
    })
}



