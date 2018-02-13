use ast_locations::Location;
use message::Message;

#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum OcamlParseError {
    #[fail(display = "{}", _0)]
    Lexer(LexerError, Location),

    #[fail(display = "{}", _0)]
    Syntax(SyntaxError),
}

impl OcamlParseError {
    /// Returns a Message for the error.
    pub fn as_msg<'a>(&'a self, src: &'a str) -> Message<'a> {
        match *self {
            OcamlParseError::Lexer(ref err, ref loc) => {
                let msg = format!("{}", err);
                loc.msg(src, msg.into())
            }
            OcamlParseError::Syntax(ref err) => err.as_msg(src),
        }
    }
}

/// An error when parsing characters into tokens.
#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum LexerError {
    #[fail(display = "illegal character: {:?}", _0)]
    IllegalCharacter(char),

    #[fail(display = "illegal escape sequence: {:?}", _0)]
    IllegalEscape(String),

    #[fail(display = "unterminated comment: {}", _0)]
    UnterminatedComment(Location),

    #[fail(display = "unterminated string")]
    UnterminatedString,

    #[fail(display = "unterminated string in comment: ({}, {})", _0, _1)]
    UnterminatedStringInComment(Location, Location),

    #[fail(display = "keyword as label: {:?}", _0)]
    KeywordAsLabel(String),

    #[fail(display = "invalid literal: {:?}", _0)]
    InvalidLiteral(String),

    #[fail(display = "invalid directive: ({:?}, {:?})", _0, _1)]
    InvalidDirective(String, Option<String>),
}

/// An error when parsing tokens into a syntax tree.
#[derive(Clone, Debug, Deserialize, Eq, Fail, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum SyntaxError {
    #[fail(display = "unclosed delimiter: {:?} expected", _3)]
    Unclosed(Location, String, Location, String),

    #[fail(display = "{} expected", _1)]
    Expecting(Location, String),

    #[fail(display = "{} not expected", _1)]
    NotExpecting(Location, String),

    #[fail(display = "applicative paths of the form F(X).t are not supported")]
    ApplicativePath(Location),

    #[fail(display = "'{} is reserved for the type {}", _1, _1)]
    VariableInScope(Location, String),

    // Thanks, OCaml...
    #[fail(display = "syntax error")]
    Other(Location),

    #[fail(display = "ill-formed ast: {}", _1)]
    IllFormedAst(Location, String),

    #[fail(display = "invalid package type: {}", _1)]
    InvalidPackageType(Location, String),
}

impl SyntaxError {
    /// Returns a Message for the error.
    pub fn as_msg<'a>(&'a self, src: &'a str) -> Message<'a> {
        let msg = format!("{}", self);
        self.location().msg(src, msg.into())
    }

    /// Gets the location associated with the error.
    pub fn location(&self) -> &Location {
        match *self {
            SyntaxError::Unclosed(_, _, ref loc, _) => loc,
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
