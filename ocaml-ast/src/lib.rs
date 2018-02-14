//! The OCaml AST, and a parser.
//!
//! The AST structures are based off of OCaml's
//! [Parsetree](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html)
//! module.
//!
//! This works by converting the nodes to JSON in OCaml, then shuffling the
//! string across the AST boundary. It's not ideal...
// #![warn(missing_docs)]

#[cfg(feature = "ansi_term")]
extern crate ansi_term;
extern crate either;
extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde_derive;
#[cfg_attr(test, macro_use)]
extern crate serde_json;

mod ast_class;
mod ast_core;
mod ast_errors;
mod ast_extension;
mod ast_locations;
mod ast_misc;
mod ast_module;
mod ast_toplevel;
mod ffi;
mod message;

#[cfg(test)]
mod tests;

use std::ffi::{CStr, CString};
use std::mem::{forget, zeroed};
use std::ptr::null;

pub use ast_class::*;
pub use ast_core::*;
pub use ast_errors::*;
pub use ast_extension::*;
pub use ast_locations::*;
pub use ast_misc::*;
pub use ast_module::*;
pub use ast_toplevel::*;
pub use message::*;

/// Errors returned from this crate.
#[derive(Debug, Fail)]
pub enum OcamlAstError {
    #[fail(display = "unexpected return value from the OCaml runtime: code {}", _0)]
    BadReturn(isize),

    #[fail(display = "ocaml-ast was compiled without the {} function. Report this as a bug!", _0)]
    MissingOCamlFunction(&'static str),

    #[fail(display = "exception from OCaml: {}", _0)]
    OcamlException(String),

    #[fail(display = "{}", _0)]
    OcamlParse(#[cause] OcamlParseError),

    #[fail(display = "{}", _0)]
    SerdeJson(#[cause] serde_json::Error),
}

impl From<OcamlParseError> for OcamlAstError {
    fn from(err: OcamlParseError) -> OcamlAstError {
        OcamlAstError::OcamlParse(err)
    }
}

fn init() -> Result<bool, OcamlAstError> {
    let ret = unsafe { ffi::ocaml_ast_init() };
    match ret {
        0 => Ok(true),
        1 => Ok(false),
        // This makes the assumption that sizeof(int) <= sizeof(void*).
        n => Err(OcamlAstError::BadReturn(n as isize)),
    }
}

/// The main parser function.
///
/// TODO: Use a real error type.
pub fn parse(
    src: &[u8],
    filename: Option<&str>,
) -> Result<Vec<ToplevelPhrase>, OcamlAstError> {
    init()?;

    // Convert src and filename to both be *const [i8] respectively.
    let src_len = src.len();
    let src = src.as_ptr() as *const i8;
    let path_len = filename.map(|f| f.len()).unwrap_or(0);
    let path = filename.map(|f| f.as_ptr() as *const i8).unwrap_or(null());

    let mut out_data = unsafe { zeroed() };
    let ret = unsafe {
        ffi::ocaml_ast_parse(src, src_len, path, path_len, &mut out_data)
    };

    let json = match ret {
        0 => {
            let s = unsafe { out_data.success };
            assert!(!s.is_null());
            let s = unsafe { CString::from_raw(s) };
            s.into_string().unwrap() // TODO Handle error
        }
        1 => {
            let s = unsafe { out_data.missing_value };
            assert!(!s.is_null());
            let s = unsafe { CStr::from_ptr::<'static>(s) };
            let s = s.to_str().unwrap(); // TODO Handle error
            return Err(OcamlAstError::MissingOCamlFunction(s));
        }
        n => {
            // out_data shouldn't have a destructor, but there's no harm to
            // being extra-safe.
            forget(out_data);

            return Err(OcamlAstError::BadReturn(n as isize));
        }
    };

    #[derive(Deserialize)]
    #[serde(tag = "type", content = "value")]
    enum FfiError {
        Lexer(LexerError, Location),
        Other(String),
        Syntax(SyntaxError),
    }

    impl From<FfiError> for OcamlAstError {
        fn from(err: FfiError) -> OcamlAstError {
            match err {
                FfiError::Lexer(err, loc) => {
                    OcamlAstError::OcamlParse(OcamlParseError::Lexer(err, loc))
                }
                FfiError::Syntax(err) => {
                    OcamlAstError::OcamlParse(OcamlParseError::Syntax(err))
                }
                FfiError::Other(err) => OcamlAstError::OcamlException(err),
            }
        }
    }

    let ast: Result<Vec<ToplevelPhrase>, FfiError> =
        serde_json::from_str(&json).map_err(OcamlAstError::SerdeJson)?;
    let mut ast = ast?;
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
    Integer(String, Option<char>),

    /// A character.
    Char(char),

    /// A string with a possible custom delimiter.
    String(String, Option<String>),

    /// A float with a possible suffix.
    Float(String, Option<char>),
}
