//! The OCaml AST, and a parser.
//!
//! The AST structures are based off of OCaml's
//! [Parsetree](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html)
//! module.
//!
//! This works by converting the nodes to JSON in OCaml, then shuffling the
//! string across the AST boundary. It's not ideal...
// #![warn(missing_docs)]

#[macro_use]
extern crate serde_derive;
#[cfg_attr(test, macro_use)]
extern crate serde_json;

mod ast_class;
mod ast_core;
mod ast_extension;
mod ast_locations;
mod ast_misc;
mod ast_module;
mod ast_toplevel;
mod ffi;

#[cfg(test)]
mod tests;

pub use ast_class::*;
pub use ast_core::*;
pub use ast_extension::*;
pub use ast_locations::*;
pub use ast_misc::*;
pub use ast_module::*;
pub use ast_toplevel::*;

/// The main parser function.
///
/// TODO: Use a real error type.
pub fn parse(
    src: &str,
    filename: Option<&str>,
) -> Result<Vec<ToplevelPhrase>, String> {
    ffi::init("ocaml_ast").unwrap();

    let mut ast: Vec<ToplevelPhrase> = ffi::parse(src, filename)
        .map_err(String::from)
        .and_then(|json| {
            let json = serde_json::to_string_pretty(
                &serde_json::from_str::<serde_json::Value>(&json).unwrap(),
            ).unwrap();
            println!("{}", json);
            serde_json::from_str(&json).map_err(|e| e.to_string())
        })?;
    ast.retain(|tlp| match *tlp {
        ToplevelPhrase::Def(ref s) => !s.is_empty(),
        _ => true,
    });
    Ok(ast)
}

/// A constant value.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Constant {
    /// An integer with a possible suffix.
    Int(String, Option<char>),

    /// A character.
    Char(char),

    /// A string with a possible custom delimiter.
    String(String, Option<String>),

    /// A float with a possible suffix.
    Float(String, Option<char>),
}
