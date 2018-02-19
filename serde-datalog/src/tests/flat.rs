use to_ast;
use ast_builder::Ast;

#[derive(Debug, Serialize)]
struct Foo {
    bar: &'static str,
    baz: i32,
}

#[test]
fn foo() {
    let foo = Foo {
        bar: "asdf",
        baz: 12,
    };
    assert_eq!(
        to_ast(&foo).unwrap(),
        Ast::from(vec![
            (
                "Foo".to_string(),
                vec!["0".to_string(), "asdf".to_string(), "12".to_string()],
            ),
        ])
    );
}

#[test]
fn foos() {
    let foos = vec![
        Foo {
            bar: "asdf",
            baz: 12,
        },
        Foo {
            bar: "qwerty",
            baz: 34,
        },
        Foo {
            bar: "zxcvbn",
            baz: 56,
        },
    ];
    assert_eq!(
        to_ast(&foos).unwrap(),
        Ast::from(vec![
            (
                "Foo".to_string(),
                vec!["0".to_string(), "asdf".to_string(), "12".to_string()],
            ),
            (
                "Foo".to_string(),
                vec!["1".to_string(), "qwerty".to_string(), "34".to_string()],
            ),
            (
                "Foo".to_string(),
                vec!["2".to_string(), "zxcvbn".to_string(), "56".to_string()],
            ),
        ])
    );
}
