use std::borrow::Cow;

use Message;

const EXAMPLE_CODE: &str = r#"Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10
Line 11
Line 12
Line 13
"#;

#[test]
fn line_10() {
    let m = Message::new(
        (9, 0),
        (9, 4),
        Cow::Borrowed("hello"),
        None,
        EXAMPLE_CODE,
    );
    assert_eq!(m.to_string(), "    | \n  8 | Line 8\n  9 | Line 9\n    | ^^^^ hello\n 10 | Line 10\n    | ");
}

#[test]
fn multiline() {
    let m =
        Message::new((2, 2), (3, 4), Cow::Borrowed("foo"), None, EXAMPLE_CODE);
    assert_eq!(
        m.to_string(),
        "   | \n 1 | Line 1\n 2 | Line 2\n   |   ^^^^\n 3 | Line 3\n   | ^^^^ foo\n 4 | Line 4\n   | "
    );
}

#[test]
fn multilines() {
    let m =
        Message::new((2, 2), (4, 4), Cow::Borrowed("bar"), None, EXAMPLE_CODE);
    assert_eq!(
        m.to_string(),
        "   | \n 1 | Line 1\n 2 | Line 2\n   |   ^^^^\n 3 | Line 3\n   | ^^^^^^\n 4 | Line 4\n   | ^^^^ bar\n 5 | Line 5\n   | "
    );
}

#[test]
fn point() {
    let m = Message::new(
        (2, 2),
        (2, 2),
        Cow::Borrowed("point!"),
        None,
        EXAMPLE_CODE,
    );
    assert_eq!(
        m.to_string(),
        "   | \n 1 | Line 1\n 2 | Line 2\n   |   ^ point!\n 3 | Line 3\n   | "
    );
}
