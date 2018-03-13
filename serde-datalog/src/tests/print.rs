use ast_builder::AstBuilder;

#[test]
fn escape() {
    let mut builder = AstBuilder::new();

    let mut fact = builder.fact("foo");
    fact.term("\\'Foo\\'");
    fact.finish();

    let ast = builder.finish();
    let printed = ast.to_string();
    assert_eq!(printed, "foo(0, '\\\\\\'Foo\\\\\\'').\n");
}
