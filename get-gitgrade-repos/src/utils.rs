use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use failure::Error;
use futures::{Async, Future};
use tokio_core::reactor::Handle;
use tokio_process::{CommandExt, OutputAsync};

/// A future for a successful `git clone` or `git pull`.
///
/// The yielded value is the path the updated repo is now at.
pub struct CloneOrPullFuture {
    inner: RunCommandFuture,

    // None iff already yielded.
    path: Option<PathBuf>,

    // None iff pull or already yielded.
    url: Option<String>,
}

impl Future for CloneOrPullFuture {
    type Item = PathBuf;
    type Error = Error;

    fn poll(&mut self) -> Result<Async<PathBuf>, Error> {
        match self.inner.poll() {
            Ok(Async::Ready(_)) => Ok(Async::Ready(self.path.take().unwrap())),
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(err) => Err(err.context({
                let path = self.path.take().unwrap();
                let path = path.display();
                if let Some(url) = self.url.take() {
                    format!("When cloning {} into {}", url, path)
                } else {
                    format!("When pulling {}", path)
                }
            }).into()),
        }
    }
}

/// Clones the given URL into the path if it doesn't exist, or pulls if it
/// does.
pub fn clone_or_pull(
    path: PathBuf,
    url: String,
    handle: &Handle,
) -> CloneOrPullFuture {
    let exists = path.exists();
    let fut = if exists {
        debug!("Pulling {}...", path.display());
        run_command("git", &["pull"], Some(&path), handle)
    } else {
        debug!("Cloning {} from {}...", path.display(), url);
        run_command(
            "git",
            &[
                OsStr::new("clone"),
                OsStr::new(url.as_str()),
                OsStr::new(&path),
            ],
            None,
            handle,
        )
    };
    CloneOrPullFuture {
        inner: fut,
        path: Some(path),
        url: if exists { Some(url) } else { None },
    }
}

/// A future for a command. See `run_command`.
pub struct RunCommandFuture(OutputAsync);

impl Future for RunCommandFuture {
    type Item = Vec<u8>;
    type Error = Error;

    fn poll(&mut self) -> Result<Async<Vec<u8>>, Error> {
        match self.0.poll() {
            Ok(Async::Ready(output)) => if output.status.success() {
                Ok(Async::Ready(output.stdout))
            } else {
                let stderr =
                    String::from_utf8_lossy(&output.stderr).to_string();
                Err(format_err!("Exited with status {}", output.status)
                    .context(stderr)
                    .into())
            },
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(err) => Err(err.into()),
        }
    }
}

/// Runs a command, returning an error if the status code was not successful,
/// and returning the contents of stdout if it was. stderr is inherited.
pub fn run_command<
    Arg: AsRef<OsStr>,
    Args: IntoIterator<Item = Arg>,
    Cmd: AsRef<OsStr>,
>(
    command: Cmd,
    args: Args,
    cwd: Option<&Path>,
    handle: &Handle,
) -> RunCommandFuture {
    lazy_static!{
        static ref DOT: PathBuf = PathBuf::from(".");
    }

    RunCommandFuture(
        Command::new(command)
            .args(args)
            .current_dir(cwd.unwrap_or(&DOT))
            .stderr(Stdio::inherit())
            .output_async(handle),
    )
}
