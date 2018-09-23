//! A command-line tool to clone and/or pull all the CSCI2041 student
//! repositories.
//!
//! This may also work for other classes that use the current (as of
//! Spring 2018) GitGrade system, but no guarantees. Try the `--github-url`
//! flag if you're trying to do this.
//!
//! Due to the nature of the current (as of Spring 2018) GitGrade system, this
//! requires an installation of Python 3 such that `python3` is the Python
//! interpreter in the `PATH` this binary is called with.
//!
//! Also, since I'm too lazy to use libgit2, requires the `git` executable to
//! be in the `PATH`.

#[macro_use]
extern crate clap;
#[macro_use]
extern crate failure;
extern crate futures;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate stderrlog;
extern crate tokio_core;
extern crate tokio_process;

mod grading_scripts;
mod utils;

use std::path::PathBuf;
use std::process::exit;

use failure::Error;
use futures::stream::futures_unordered;
use futures::{Future, Stream};
use tokio_core::reactor::Core;

use grading_scripts::get_student_uiids;
use utils::clone_or_pull;

fn main() {
    // Parse CLI arguments.
    let matches = clap_app!((crate_name!()) =>
        (about: "Clones and/or pulls all the CSCI2041 student repositories.")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg REPO_DIR: "The directory to clone repos into. Defaults to `gitgrade/repos'.")
        (@arg GITHUB_URL: --("github-url") +takes_value "The GitHub base URL to use. Defaults to `git@github.umn.edu:umn-csci-2041-S18/'.")
        (@arg GRADING_SCRIPTS_DIR: --("grading-scripts") +takes_value "The directory to clone/pull the grading scripts into. Defaults to `gitgrade/grading-scripts'.")
        (@arg QUIET: -q --quiet "Silence all logging output.")
        (@arg VERBOSE: -v --verbose +multiple "Increase the logging verbosity.")
    ).get_matches();
    let repo_dir = PathBuf::from(
        matches
            .value_of("REPO_DIR")
            .unwrap_or_else(|| "gitgrade/repos".into()),
    );
    let github_url = matches
        .value_of("GITHUB_URL")
        .unwrap_or_else(|| "git@github.umn.edu:umn-csci-2041-S18/")
        .to_string();
    let grading_scripts_dir = PathBuf::from(
        matches
            .value_of("GRADING_SCRIPTS_DIR")
            .unwrap_or_else(|| "gitgrade/grading-scripts".into()),
    );

    // Set up logging.
    stderrlog::new()
        .quiet(matches.is_present("QUIET"))
        .verbosity(matches.occurrences_of("VERBOSE") as usize + 2)
        .init()
        .unwrap();

    // Start the "real" main.
    match run(repo_dir, github_url, grading_scripts_dir) {
        Ok(0) => {}
        Ok(n) => exit(n),
        Err(err) => if log_enabled!(log::Level::Error) {
            for err in err.causes() {
                error!("{}", err);
            }
            exit(1);
        } else {
            panic!("{}", err);
        },
    }
}

fn run(
    repo_dir: PathBuf,
    github_url: String,
    grading_scripts_dir: PathBuf,
) -> Result<i32, Error> {
    let mut core = Core::new()?;
    let student_uiids =
        get_student_uiids(&github_url, grading_scripts_dir, &mut core)?;
    info!("Pulling {} repos...", student_uiids.len());
    core.run(
        futures_unordered(student_uiids.into_iter().map(|uiid| {
            let path = repo_dir.join(&format!("repo-{}", uiid));
            let url = format!("{}repo-{}.git", github_url, uiid);
            clone_or_pull(path, url).then(Ok)
        })).collect()
        .and_then(|res| {
            let mut ok = 0;
            let mut err = 0;
            for r in res {
                match r {
                    Ok(_) => {
                        ok += 1;
                    }
                    Err(e) => {
                        for err in e.causes() {
                            error!("{}", err);
                        }
                        err += 1;
                    }
                }
            }
            info!("Successfully cloned {} repos.", ok);
            if err != 0 {
                warn!("{} repos could not be cloned.", err);
            }
            Ok(err)
        }),
    )
}
