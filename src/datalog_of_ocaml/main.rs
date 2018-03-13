#[macro_use]
extern crate clap;
extern crate ocaml_ast;
extern crate serde_datalog;

use std::fmt::Display;
use std::io::Error as IoError;
use std::process::exit;

use ocaml_ast::{parse, OcamlAstError};
use serde_datalog::Data;
use serde_datalog::ast_builder::AstBuilder;

fn main() {
    let matches = clap_app!((crate_name!()) =>
        (about: "Parses an OCaml file, printing the output as a set of Datalog facts. This should also work as a set of Prolog facts.")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg FILE: "The OCaml code to print")
        (@arg OUT_FILE: -o --output +takes_value "The file to output to")
    ).get_matches();

    let file = matches.value_of("FILE");
    let src = read_from(file).unwrap_or_else(|err| {
        eprintln!("Couldn't read from {}", file.unwrap_or("stdin"));
        eprintln!("{}", err);
        exit(1);
    });

    let ast = parse(&src, file).unwrap_or_else(|err| {
        match err {
            OcamlAstError::OcamlParse(err) => {
                let src = String::from_utf8_lossy(&src);
                eprintln!("{}", err.as_msg(&src).with_color());
            }
            _ => {
                eprintln!("Couldn't parse {}", file.unwrap_or("stdin"));
                eprintln!("{}", err);
            }
        }
        exit(1);
    });

    let mut builder = AstBuilder::new();
    let mut fact = builder.fact("src");
    fact.term(file.unwrap_or(""));
    fact.term(String::from_utf8_lossy(&src));
    match serde_datalog::to_data(&ast).unwrap() {
        Data::Seq(data) | Data::Tuple(data) => for d in data {
            d.add_to_ast(&mut builder);
        },
        data => {
            data.add_to_ast(&mut builder);
        }
    }
    write_to(matches.value_of("OUT_FILE"), builder.finish()).unwrap_or_else(
        |err| {
            eprintln!("Couldn't write to {}", file.unwrap_or("stdout"));
            eprintln!("{}", err);
            exit(1);
        },
    )
}

fn read_from(path: Option<&str>) -> Result<Vec<u8>, IoError> {
    use std::io::Read;

    let mut buf = Vec::new();
    let _: usize = match path {
        Some(f) => {
            use std::fs::File;
            File::open(f)?.read_to_end(&mut buf)?
        }
        None => {
            use std::io::stdin;
            stdin().read_to_end(&mut buf)?
        }
    };
    Ok(buf)
}

fn write_to<T: Display>(path: Option<&str>, data: T) -> Result<(), IoError> {
    use std::io::Write;

    match path {
        Some(f) => {
            use std::fs::File;
            writeln!(File::create(f)?, "{}", data)
        }
        None => {
            use std::io::stdout;
            writeln!(stdout(), "{}", data)
        }
    }
}
