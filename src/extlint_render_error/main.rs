extern crate failure;
extern crate pretty_errors;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
#[macro_use]
extern crate structopt;

use std::borrow::Cow;
use std::fs::File;
use std::path::PathBuf;

use failure::Error;
use pretty_errors::Message;
use structopt::StructOpt;

fn main() {
    let options = Options::from_args();
    if let Err(err) = run(options) {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn run(options: Options) -> Result<(), Error> {
    let Input { lints, path, src } = {
        let f = File::open(&options.file)?;
        serde_json::from_reader(f)?
    };

    let output = lints
        .into_iter()
        .map(|lint| process(&path, &src, lint))
        .collect::<Result<Vec<_>, _>>()?;

    let f = File::create(options.file)?;
    serde_json::to_writer(f, &output)?;

    Ok(())
}

fn process(
    path: &str,
    src: &str,
    input: InputLint,
) -> Result<OutputLint, Error> {
    let pretty = {
        let msg = Message::new(
            input.start,
            input.end,
            Cow::Borrowed(&input.name),
            Some(path),
            src,
        );
        format!("{}{}\n", msg, input.desc)
    };
    Ok(OutputLint {
        desc: input.desc,
        name: input.name,
        points_lost: input.points_lost,
        pretty,
        start: input.start,
        end: input.end,
    })
}

#[derive(Debug, Deserialize)]
struct Input {
    lints: Vec<InputLint>,
    path: String,
    src: String,
}

#[derive(Debug, Deserialize)]
struct InputLint {
    desc: String,
    name: String,
    points_lost: usize,
    start: (usize, usize),
    end: (usize, usize),
}

#[derive(Debug, Serialize)]
struct OutputLint {
    desc: String,
    name: String,
    points_lost: usize,
    pretty: String,
    start: (usize, usize),
    end: (usize, usize),
}

/// Reads the JSON file produced by the Prolog parts of extlint, and adds a
/// `pretty` field containing a pretty error.
#[derive(Debug, StructOpt)]
#[structopt(raw(setting = "structopt::clap::AppSettings::ColoredHelp"))]
struct Options {
    /// The input JSON file.
    #[structopt(parse(from_os_str))]
    file: PathBuf,
}
