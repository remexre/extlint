macro_rules! parse_test {
    ($name:ident) => {
        mod $name {
            use {parse, ToplevelPhrase};
            use serde_json::from_str;

            const OCAML_PATH: &'static str = parse_test!(@path $name.ml);
            const OCAML_SRC: &'static str = parse_test!(@file $name.ml);
            const JSON_SRC: &'static str = parse_test!(@file $name.json);

            #[test]
            fn parse_test() {
                let ocaml_ast = parse(OCAML_SRC, Some(OCAML_PATH)).unwrap();
                let json_ast: Vec<ToplevelPhrase> = from_str(JSON_SRC).unwrap();
                assert_eq!(ocaml_ast, json_ast);
            }
        }
    };
    (@file $name:ident . $ext:ident) => {
        include_str!(parse_test!(@path $name . $ext))
    };
    (@path $name:ident . $ext:ident) => {
        concat!("data/", stringify!($name), ".", stringify!($ext))
    };
}

parse_test!(hello_world);
