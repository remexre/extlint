#[macro_use]
extern crate clap;
extern crate ocaml_ast;
extern crate serde_datalog;

use std::io::Error as IoError;
use std::process::exit;

use ocaml_ast::{parse, OcamlAstError};

fn main() {
    let matches = clap_app!((crate_name!()) =>
        (about: "Parses an OCaml file, printing the output as a set of Datalog facts. This should also work as a set of Prolog facts.")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg FILE: "The OCaml code to print")
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
                eprintln!("{}", err.as_msg(&src));
            }
            _ => {
                eprintln!("Couldn't parse {}", file.unwrap_or("stdin"));
                eprintln!("{}", err);
            }
        }
        exit(1);
    });

    let ast = serde_datalog::to_ast(&ast).unwrap();
    println!("{}", ast);
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
