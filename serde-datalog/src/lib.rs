//! A serializer into Datalog literals.
#![warn(missing_docs)]

extern crate serde;
#[cfg(test)]
#[macro_use]
extern crate serde_derive;

pub mod ast_builder;
mod data;
mod print;
mod simplify;

#[cfg(test)]
mod tests;

use std::fmt::{Display, Formatter, Result as FmtResult};

use serde::Serialize;

use ast_builder::Ast;

/// Serializes a value into Datalog literals.
pub fn to_ast<T: Serialize>(value: &T) -> Result<Ast, SerializeError> {
    value
        .serialize(simplify::Serializer)
        .map(|data| data.to_ast())
}

/// An error during serialization.
#[derive(Clone, Debug, PartialEq)]
pub enum SerializeError {
    /// A custom error from serialization.
    Custom(String),
}

impl Display for SerializeError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            SerializeError::Custom(ref s) => fmt.write_str(s),
        }
    }
}

impl serde::ser::Error for SerializeError {
    fn custom<T: std::fmt::Display>(msg: T) -> SerializeError {
        SerializeError::Custom(msg.to_string())
    }
}

impl std::error::Error for SerializeError {
    fn description(&self) -> &str {
        match *self {
            SerializeError::Custom(ref s) => s,
        }
    }
}
