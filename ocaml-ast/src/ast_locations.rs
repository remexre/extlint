use std::fmt::{Display, Formatter, Result as FmtResult};

/// A location-annotated value.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Loc<T> {
    pub txt: T,
    pub location: Location,
}

/// A span of source code.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Location {
    /// The starting position.
    pub start: Position,

    /// The ending position.
    pub end: Position,

    /// Whether the location is a ghost location.
    pub ghost: bool,
}

impl Location {
    /// Attaches a message to the Location.
    pub fn msg<'a>(
        &'a self,
        src: &'a str,
        msg: Option<&'a str>,
    ) -> Message<'a> {
        Message {
            start: (self.start.line, self.start.column),
            end: Some((self.end.line, self.end.column)),
            msg,
            src,
        }
    }
}

impl Display for Location {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if self.ghost {
            write!(fmt, "???")
        } else {
            write!(
                fmt,
                "{}: {}:{} -- {}:{}",
                self.start.filename,
                self.start.line,
                self.start.column,
                self.end.line,
                self.end.column
            )
        }
    }
}

/// A helper struct for rendering a message attached to a span of source code.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Message<'a> {
    pub start: (usize, usize),
    pub end: Option<(usize, usize)>,
    pub msg: Option<&'a str>,
    pub src: &'a str,
}

impl<'a> Display for Message<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        unimplemented!()
    }
}

/// A point in source code.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Position {
    /// The filename.
    pub filename: String,

    /// The line number.
    pub line: usize,

    /// The column number.
    pub column: usize,

    /// The position, in characters from the start of the file.
    pub offset: usize,
}

impl Position {
    /// Attaches a message to the Position.
    pub fn msg<'a>(
        &'a self,
        src: &'a str,
        msg: Option<&'a str>,
    ) -> Message<'a> {
        Message {
            start: (self.line, self.column),
            end: None,
            msg,
            src,
        }
    }
}

impl Display for Position {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}: {}:{}", self.filename, self.line, self.column)
    }
}
