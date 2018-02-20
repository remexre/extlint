use ast_builder::{Ast, AstBuilder, FactBuilder};

/// A normalized value being serialized.
#[derive(Debug)]
pub enum Data {
    /// A primitive value, which may be serialized as either an atom or a fact.
    Prim(&'static str, String),

    /// A sequence of `cons` and `nil`.
    Seq(Vec<Data>),

    /// A struct.
    Struct(String, Vec<Data>),

    /// A tuple, which is inlined into a term list.
    Tuple(Vec<Data>),
}

impl Data {
    /// Converts the data to a full Datalog program.
    pub fn to_ast(self) -> Ast {
        let mut ast = AstBuilder::new();
        match self {
            Data::Seq(data) | Data::Tuple(data) => for d in data {
                d.add_to_ast(&mut ast);
            },
            data => {
                data.add_to_ast(&mut ast);
            }
        }
        ast.finish()
    }

    /// Adds the data to an AST.
    pub fn add_to_ast(self, ast: &mut AstBuilder) -> usize {
        match self {
            Data::Prim(name, val) => {
                let mut fact = ast.fact(name);
                fact.term(val);
                fact.finish()
            }
            Data::Seq(mut vals) => {
                let mut id = ast.fact("nil").finish();
                while let Some(val) = vals.pop() {
                    let mut fact = ast.fact("cons");
                    val.add_to_fact(ast, &mut fact);
                    fact.term(id.to_string());
                    id = fact.finish();
                }
                id
            }
            Data::Struct(name, vals) => {
                let mut fact = ast.fact(name);
                for val in vals {
                    val.add_to_fact(ast, &mut fact);
                }
                fact.finish()
            }
            Data::Tuple(vals) => {
                let mut fact = ast.fact("tuple");
                for val in vals {
                    val.add_to_fact(ast, &mut fact);
                }
                fact.finish()
            }
        }
    }

    /// Adds the data to a fact.
    pub fn add_to_fact(self, ast: &mut AstBuilder, fact: &mut FactBuilder) {
        match self {
            Data::Prim(_name, val) => {
                fact.term(val);
            }
            Data::Tuple(vals) => for val in vals {
                val.add_to_fact(ast, fact);
            },
            data => {
                let id = data.add_to_ast(ast);
                fact.term(id.to_string());
            }
        }
    }
}
