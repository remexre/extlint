#[macro_use]
extern crate clap;
extern crate ocaml_ast;

use std::fs::File;
use std::io::Read;
use std::process::exit;

use ocaml_ast::{parse, OcamlAstError};

fn main() {
    let matches = clap_app!((crate_name!()) =>
        (about: "Checks an OCaml file for syntax errors, printing them much better than the default toplevel.")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg FILE: +required "The OCaml file to check")
    ).get_matches();

    let file = matches.value_of("FILE").unwrap();
    let mut src = Vec::new();
    File::open(file)
        .and_then(|mut f| f.read_to_end(&mut src))
        .unwrap_or_else(|err| {
            eprintln!("Couldn't read from {}", file);
            eprintln!("{}", err);
            exit(1);
        });

    if let Some(err) = parse(&src, Some(file)).err() {
        match err {
            OcamlAstError::OcamlParse(err) => {
                let src = String::from_utf8_lossy(&src);
                eprintln!("{}", err.as_msg(&src));
            }
            _ => {
                eprintln!("Couldn't parse {}", file);
                eprintln!("{}", err);
            }
        }
        exit(1);
    }
}
