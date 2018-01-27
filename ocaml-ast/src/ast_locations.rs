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
