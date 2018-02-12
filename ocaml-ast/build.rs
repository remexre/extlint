extern crate cc;
#[macro_use]
extern crate lazy_static;

use std::env::var;
use std::fs::remove_file;
use std::path::PathBuf;
use std::process::{exit, Command};

const SWITCH_STANDARD: &str = "4.06.0";
const SWITCH_MUSL: &str = "4.06.0+musl+static+flambda";

lazy_static! {
    static ref SWITCH: String = {
        println!("cargo:rerun-if-env-changed=OPAMSWITCH");
        if let Ok(switch) = var("OPAMSWITCH") {
            switch
        } else {
            let s = if var("TARGET").unwrap() == "x86_64-unknown-linux-musl" {
                SWITCH_MUSL
            } else {
                SWITCH_STANDARD
            };
            s.to_string()
        }
    };
    static ref OUT_DIR: String = {
        println!("cargo:rerun-if-env-changed=OUT_DIR");
        var("OUT_DIR").unwrap()
    };
}

fn main() {
    remove_file(PathBuf::from(&*OUT_DIR).join("ocaml_ast.o")).ok();

    let c_files = ["ocaml-src/main.c"];

    for src in c_files.iter() {
        println!("cargo:rerun-if-changed={}", src);
    }

    let include_path = ocamlopt(&["-where"]);

    cc::Build::new()
        .files(c_files.iter())
        .object(compile_ocaml("ocaml_ast"))
        .include(include_path)
        .warnings(true)
        .compile("ocaml_ast");
    println!("cargo:rustc-link-search=native={}", *OUT_DIR);
    println!("cargo:rustc-link-lib=static=ocaml_ast");
}

fn compile_ocaml(base: &str) -> PathBuf {
    let src_file = format!("ocaml-src/{}.ml", base);
    println!("cargo:rerun-if-changed={}", src_file);
    let obj_file = format!("{}/{}.o", *OUT_DIR, base);

    ocamlopt(&[
        "-linkpkg",
        "-o",
        &obj_file,
        "-output-complete-obj",
        "-package",
        "compiler-libs.common",
        &src_file,
    ]);
    PathBuf::from(obj_file)
}

fn ocamlopt(args: &[&str]) -> String {
    println!("{:#?}", args);
    let o = Command::new("opam")
        .arg("config")
        .arg("exec")
        .arg("--switch")
        .arg(&*SWITCH)
        .arg("--")
        .arg("ocamlfind")
        .arg("ocamlopt")
        .args(args)
        .output()
        .unwrap();
    for l in String::from_utf8_lossy(&o.stderr).lines() {
        println!("cargo:warning={}", l);
    }
    if !o.status.success() {
        exit(o.status.code().unwrap_or(1))
    }
    String::from_utf8_lossy(&o.stdout).trim().to_string()
}
