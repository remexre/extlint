use ast_locations::Location;

#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum OcamlParseError {
    #[fail(display = "{}", _0)] Lexer(LexerError, Location),

    #[fail(display = "{}", _0)] Syntax(SyntaxError),
}

/// An error when parsing characters into tokens.
#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum LexerError {
    #[fail(display = "illegal character: {:?}", _0)] IllegalCharacter(char),

    #[fail(display = "illegal escape sequence: {:?}", _0)] IllegalEscape(String),

    #[fail(display = "unterminated comment: {}", _0)]
    UnterminatedComment(Location),

    #[fail(display = "unterminated string")] UnterminatedString,

    #[fail(display = "unterminated string in comment: ({}, {})", _0, _1)]
    UnterminatedStringInComment(Location, Location),

    #[fail(display = "keyword as label: {:?}", _0)] KeywordAsLabel(String),

    #[fail(display = "invalid literal: {:?}", _0)] InvalidLiteral(String),

    #[fail(display = "invalid directive: ({:?}, {:?})", _0, _1)]
    InvalidDirective(String, Option<String>),
}

/// An error when parsing tokens into a syntax tree.
#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum SyntaxError {
    #[fail(display = "unclosed delimiter: ({}, {:?}, {}, {:?})", _0, _1, _2,
           _3)]
    Unclosed(Location, String, Location, String),

    #[fail(display = "expected: ({}, {:?})", _0, _1)]
    Expecting(Location, String),

    #[fail(display = "unexpected: ({}, {:?})", _0, _1)]
    NotExpecting(Location, String),

    #[fail(display = "applicative path: {}", _0)] ApplicativePath(Location),

    #[fail(display = "variable in scope: ({}, {:?})", _0, _1)]
    VariableInScope(Location, String),

    #[fail(display = "other error: {}", _0)] Other(Location),

    #[fail(display = "ill-formed ast: ({}, {:?})", _0, _1)]
    IllFormedAst(Location, String),

    #[fail(display = "invalid package type: ({}, {:?})", _0, _1)]
    InvalidPackageType(Location, String),
}

impl SyntaxError {
    /// Gets the location associated with the error.
    pub fn location(&self) -> &Location {
        match *self {
            SyntaxError::Unclosed(ref loc, _, _, _) => loc,
            SyntaxError::Expecting(ref loc, _) => loc,
            SyntaxError::NotExpecting(ref loc, _) => loc,
            SyntaxError::ApplicativePath(ref loc) => loc,
            SyntaxError::VariableInScope(ref loc, _) => loc,
            SyntaxError::Other(ref loc) => loc,
            SyntaxError::IllFormedAst(ref loc, _) => loc,
            SyntaxError::InvalidPackageType(ref loc, _) => loc,
        }
    }
}
