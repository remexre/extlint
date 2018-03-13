#[macro_use]
extern crate clap;
extern crate ocaml_ast;
extern crate serde_json;

use std::io::Error as IoError;
use std::process::exit;

use ocaml_ast::{parse, OcamlAstError};

fn main() {
    let matches = clap_app!((crate_name!()) =>
        (about: "Parses an OCaml file, printing the output as JSON")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg FILE: "The OCaml code to print")
        (@arg SILENT: -s --silent "Don't actually output JSON, only validate the OCaml file")
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
                if !matches.is_present("SILENT") {
                    let src = String::from_utf8_lossy(&src);
                    eprintln!("{}", err.as_msg(&src).with_color());
                }
            }
            _ => {
                eprintln!("Couldn't parse {}", file.unwrap_or("stdin"));
                eprintln!("{}", err);
            }
        }
        exit(1);
    });

    if !matches.is_present("SILENT") {
        use std::io::stdout;
        serde_json::to_writer(stdout(), &ast).unwrap()
    }
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
