extern crate cc;
#[macro_use]
extern crate lazy_static;

use std::fs::remove_file;
use std::path::PathBuf;
use std::process::{exit, Command};

lazy_static! {
    static ref OUT_DIR: String = {
        use std::env::var;
        var("OUT_DIR").unwrap()
    };
}

fn main() {
    remove_file(PathBuf::from(&*OUT_DIR).join("ocaml_ast.o")).ok();

    let c_files = ["ocaml-src/main.c"];

    for src in c_files.iter() {
        println!("cargo:rerun-if-changed={}", src);
    }

    let include_path = {
        let o = Command::new("ocamlfind")
            .arg("ocamlopt")
            .arg("-where")
            .output()
            .unwrap();
        for l in String::from_utf8_lossy(&o.stderr).lines() {
            println!("cargo:warning={}", l);
        }
        if !o.status.success() {
            exit(o.status.code().unwrap_or(1))
        }
        String::from_utf8_lossy(&o.stdout).trim().to_string()
    };

    cc::Build::new()
        .files(c_files.iter())
        .object(compile_ocaml("ocaml_ast"))
        .include(include_path)
        .warnings(true)
        .compile("ocaml_ast");
}

fn compile_ocaml(base: &str) -> PathBuf {
    let src_file = format!("ocaml-src/{}.ml", base);
    println!("cargo:rerun-if-changed={}", src_file);
    let obj_file = format!("{}/{}.o", *OUT_DIR, base);

    let status = Command::new("ocamlfind")
        .arg("ocamlopt")
        .arg("-linkpkg")
        .arg("-o")
        .arg(&obj_file)
        .arg("-output-complete-obj")
        .arg("-package")
        .arg("compiler-libs.common")
        .arg(src_file)
        .status()
        .unwrap();
    if !status.success() {
        exit(status.code().unwrap_or(1))
    }

    PathBuf::from(obj_file)
}
