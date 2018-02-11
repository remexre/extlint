use std::path::PathBuf;

use failure::Error;
use futures::Future;
use tokio_core::reactor::Core;

use utils::{clone_or_pull, run_command};

pub fn get_student_uiids(
    github_url: &str,
    grading_scripts_dir: PathBuf,
    core: &mut Core,
) -> Result<Vec<String>, Error> {
    let clone_url = format!("{}{}", github_url, "grading-scripts.git");
    let handle = core.handle();
    core.run(
        clone_or_pull(grading_scripts_dir, clone_url, &handle).and_then(
            |grading_scripts_dir| {
                run_command(
                    "python",
                    &["-c", "import Course; list(map(print, Course.Course().ugrad_uiids))"],
                    Some(&grading_scripts_dir),
                    &handle,
                ).map(|stdout| {
                    String::from_utf8_lossy(&stdout).lines().map(|s| s.to_string()).collect()
                })
            },
        ),
    )
}
