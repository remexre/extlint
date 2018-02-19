use ast_builder::AstBuilder;

#[test]
fn basic() {
    let mut builder = AstBuilder::new();

    let mut fact = builder.fact("foo");
    fact.term("bar");
    fact.finish();

    let mut fact = builder.fact("foo");
    fact.term("baz");
    fact.finish();

    let mut fact = builder.fact("quux");
    fact.term("xyzzy");
    fact.finish();

    let mut fact = builder.fact("quux");
    fact.term("1");
    fact.term("2");
    fact.finish();

    let mut fact = builder.fact("zxcvbn");
    fact.term("1 2");
    fact.finish();

    let ast = builder.finish();
    let printed = ast.to_string();
    assert_eq!(
        printed,
        r#"foo(0, bar).
foo(1, baz).

quux(2, xyzzy).
quux(3, 1, 2).

zxcvbn(4, '1 2').
"#
    );
}
