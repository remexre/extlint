use ast_misc::LongIdent;
use ast_module::Structure;

/// An entry at the top level.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ToplevelPhrase {
    /// A definition.
    Def(Structure),

    /// A directive.
    Dir(String, DirectiveArgument),
}

/// An argument to a top-level directive.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum DirectiveArgument {
    None,
    String(String),
    // | Pdir_int(c, s) -> failwith "Pdir_int"
    Ident(LongIdent),
    Bool(bool),
}
