use {Constant, Location, Position};

macro_rules! serde_tests {
    ($t:ty, $ser:ident, $de:ident, $(($l:expr, $r:tt)),* $(,)*) => {
        #[test]
        fn $ser() {
            serde_tests!(@expr ser, $t, $(($l, $r)),*);
        }

        #[test]
        fn $de() {
            serde_tests!(@expr de, $t, $(($l, $r)),*);
        }
    };
    (@expr $test:ident, $t:ty, $(($l:expr, $r:tt)),*) => {
        $( serde_tests!(@$test $t, $l, $r); )*
    };
    (@ser $t:ty, $l:expr, $r:tt) => {
        assert_eq!(::serde_json::to_value($l).unwrap(), json!($r));
    };
    (@de $t:ty, $l:expr, $r:tt) => {
        assert_eq!(::serde_json::from_value::<$t>(json!($r)).unwrap(), $l);
    };
}

serde_tests!(Constant, constant_serialize, constant_deserialize,
    (Constant::Integer("42".to_string(), None), {
        "type": "Integer",
        "value": ["42", null],
    }),
    (Constant::Integer("42".to_string(), Some('L')), {
        "type": "Integer",
        "value": ["42", "L"],
    }),

    (Constant::Char('x'), {
        "type": "Char",
        "value": "x",
    }),

    (Constant::String("foo".to_string(), None), {
        "type": "String",
        "value": ["foo", null],
    }),
    (Constant::String("foo".to_string(), Some("de".to_string())), {
        "type": "String",
        "value": ["foo", "de"],
    }),

    (Constant::Float("3.14".to_string(), None), {
        "type": "Float",
        "value": ["3.14", null],
    }),
    (Constant::Float("173e0".to_string(), Some('f')), {
        "type": "Float",
        "value": ["173e0", "f"],
    }),
);

serde_tests!(Location, location_serialize, location_deserialize,
    (Location {
        start: Position {
            filename: "foo.ml".to_string(),
            line: 10,
            column: 4,
            offset: 125,
        },
        end: Position {
            filename: "foo.ml".to_string(),
            line: 10,
            column: 7,
            offset: 128,
        },
        ghost: false,
    }, {
        "start": {
            "filename": "foo.ml",
            "line": 10,
            "column": 4,
            "offset": 125,
        },
        "end": {
            "filename": "foo.ml",
            "line": 10,
            "column": 7,
            "offset": 128,
        },
        "ghost": false,
    })
);

serde_tests!(Position, position_serialize, position_deserialize,
    (Position {
        filename: "foo.ml".to_string(),
        line: 10,
        column: 4,
        offset: 125,
    }, {
        "filename": "foo.ml",
        "line": 10,
        "column": 4,
        "offset": 125,
    })
);
