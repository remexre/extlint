#[macro_use]
extern crate clap;
extern crate ocaml_ast;
extern crate serde_json;

use std::io::Error as IoError;

use ocaml_ast::parse;

fn main() {
    let matches = clap_app!((crate_name!()) =>
        (about: "Parses an OCaml file, printing the output as JSON")
        (author: crate_authors!())
        (version: crate_version!())
        (@arg FILE: "The OCaml code to print")
    ).get_matches();

    let file = matches.value_of("FILE");
    let src = read_from(file).unwrap_or_else(|err| {
        panic!("Couldn't read from {}: {}", file.unwrap_or("stdin"), err)
    });

    let ast = parse(&src, file).unwrap_or_else(|err| {
        panic!("Couldn't parse {}: {}", file.unwrap_or("stdin"), err)
    });
    println!("{:?}", ast)
}

fn read_from(path: Option<&str>) -> Result<String, IoError> {
    use std::io::Read;

    let mut buf = String::new();
    let _: usize = match path {
        Some(f) => {
            use std::fs::File;
            File::open(f)?.read_to_string(&mut buf)?
        }
        None => {
            use std::io::stdin;
            stdin().read_to_string(&mut buf)?
        }
    };
    Ok(buf)
}
