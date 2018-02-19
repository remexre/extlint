use ast_builder::{Ast, AstBuilder, FactBuilder};

#[derive(Debug)]
pub enum Data {
    Prim(&'static str, String),
    Seq(Vec<Data>),
    Map(Vec<(Data, Data)>),
    Struct(String, Vec<Data>),
}

impl Data {
    /// Converts the data to a full Datalog program.
    pub fn to_ast(self) -> Ast {
        let mut ast = AstBuilder::new();
        match self {
            Data::Seq(data) => for d in data {
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
                let mut id = {
                    let mut fact = ast.fact("seq");
                    fact.term("nil");
                    fact.finish()
                };
                while let Some(val) = vals.pop() {
                    let mut fact = ast.fact("seq");
                    fact.term("cons");
                    val.add_to_fact(ast, &mut fact);
                    fact.term(id.to_string());
                    id = fact.finish();
                }
                id
            }
            Data::Map(mut kvs) => {
                let mut id = {
                    let mut fact = ast.fact("map");
                    fact.term("nil");
                    fact.finish()
                };
                while let Some((k, v)) = kvs.pop() {
                    let mut fact = ast.fact("map");
                    fact.term("cons");
                    k.add_to_fact(ast, &mut fact);
                    v.add_to_fact(ast, &mut fact);
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
        }
    }

    /// Adds the data to a fact.
    pub fn add_to_fact(self, ast: &mut AstBuilder, fact: &mut FactBuilder) {
        match self {
            Data::Prim(_name, val) => {
                fact.term(val);
            }
            data => {
                let id = data.add_to_ast(ast);
                fact.term(id.to_string());
            }
        }
    }
}
