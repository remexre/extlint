macro_rules! parse_test {
    ($name:ident) => {
        mod $name {
            use {parse, ToplevelPhrase};
            use serde_json::from_slice;

            const OCAML_PATH: &str = parse_test!(@path $name.ml);
            const OCAML_SRC: &[u8] = parse_test!(@file $name.ml);
            const JSON_SRC: &[u8] = parse_test!(@file $name.json);

            #[test]
            fn parse_test() {
                let ocaml_ast = parse(OCAML_SRC, Some(OCAML_PATH)).unwrap();
                let json_ast: Vec<ToplevelPhrase> = from_slice(JSON_SRC).unwrap();
                assert_eq!(ocaml_ast, json_ast);
            }
        }
    };
    (@file $name:ident . $ext:ident) => {
        include_bytes!(parse_test!(@path $name . $ext))
    };
    (@path $name:ident . $ext:ident) => {
        concat!("data/", stringify!($name), ".", stringify!($ext))
    };
}

parse_test!(hello_world);
