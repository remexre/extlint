use std::borrow::Cow;
use std::cmp::max;
use std::fmt::{Display, Formatter, Result as FmtResult};

use message::Message;

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
    pub fn msg<'a>(&'a self, src: &'a str, msg: Cow<'a, str>) -> Message<'a> {
        let start_col = max(self.start.column, 0) as usize;
        let end_col = max(self.end.column, 0) as usize;
        Message::new(
            (self.start.line, start_col),
            (self.end.line, end_col),
            msg,
            if self.start.filename == "" {
                None
            } else {
                Some(&self.start.filename)
            },
            src,
        )
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

/// A point in source code.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Position {
    /// The filename.
    pub filename: String,

    /// The line number.
    pub line: usize,

    /// The column number.
    pub column: isize,

    /// The position, in characters from the start of the file.
    pub offset: isize,
}

impl Position {
    /// Attaches a message to the Position.
    pub fn msg<'a>(&'a self, src: &'a str, msg: Cow<'a, str>) -> Message<'a> {
        let col = max(self.column, 0) as usize;
        Message::new(
            (self.line, col),
            (self.line, col + 1),
            msg,
            if self.filename == "" {
                None
            } else {
                Some(&self.filename)
            },
            src,
        )
    }
}

impl Display for Position {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}: {}:{}", self.filename, self.line, self.column)
    }
}
