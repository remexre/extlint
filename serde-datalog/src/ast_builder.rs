//! Types for working with the Datalog AST.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

/// A Datalog AST.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ast(pub BTreeMap<String, Vec<Vec<String>>>);

impl Ast {
    /// Creates a new, empty AST.
    pub fn new() -> Ast {
        Ast(BTreeMap::new())
    }
}

impl From<Ast> for BTreeMap<String, Vec<Vec<String>>> {
    fn from(ast: Ast) -> BTreeMap<String, Vec<Vec<String>>> {
        ast.0
    }
}

impl From<Ast> for Vec<(String, Vec<String>)> {
    fn from(ast: Ast) -> Vec<(String, Vec<String>)> {
        let mut vec = Vec::new();
        for (functor, facts) in ast.0 {
            for terms in facts {
                vec.push((functor.clone(), terms));
            }
        }
        vec
    }
}

impl From<BTreeMap<String, Vec<Vec<String>>>> for Ast {
    fn from(m: BTreeMap<String, Vec<Vec<String>>>) -> Ast {
        Ast(m)
    }
}

impl From<Vec<(String, Vec<String>)>> for Ast {
    fn from(v: Vec<(String, Vec<String>)>) -> Ast {
        let mut m = BTreeMap::new();
        for (functor, terms) in v {
            m.entry(functor).or_insert(Vec::new()).push(terms);
        }
        m.into()
    }
}

/// A struct for building a Datalog program.
pub struct AstBuilder {
    inner: Rc<RefCell<AstBuilderInner>>,
}

impl AstBuilder {
    /// Creates an `AstBuilder`.
    pub fn new() -> AstBuilder {
        AstBuilder {
            inner: Rc::new(RefCell::new(AstBuilderInner {
                program: BTreeMap::new(),
                n: 0,
            })),
        }
    }

    /// Builds a new fact.
    pub fn fact<S: AsRef<str>>(&mut self, functor: S) -> FactBuilder {
        let mut inner = self.inner.borrow_mut();
        let n = inner.n;
        inner.n += 1;

        let functor = functor.as_ref().to_string();
        let facts = inner.program.entry(functor.clone()).or_insert(Vec::new());
        let fact_idx = facts.len();
        facts.push(vec![n.to_string()]);

        FactBuilder {
            inner: self.inner.clone(),
            functor,
            fact_idx,
            n,
        }
    }

    /// Finishes building the AST.
    pub fn finish(self) -> Ast {
        let inner = self.inner.borrow();
        Ast(inner.program.clone())
    }
}

struct AstBuilderInner {
    program: BTreeMap<String, Vec<Vec<String>>>,
    n: usize,
}

/// A struct for building facts.
pub struct FactBuilder {
    inner: Rc<RefCell<AstBuilderInner>>,
    fact_idx: usize,
    functor: String,
    n: usize,
}

impl FactBuilder {
    /// Adds a term to the fact.
    pub fn term<S: AsRef<str>>(&mut self, term: S) {
        let mut inner = self.inner.borrow_mut();
        inner.program.get_mut(&self.functor).unwrap()[self.fact_idx]
            .push(term.as_ref().to_string());
    }

    /// Finishes building the fact, returning the ID of the fact.
    pub fn finish(self) -> usize {
        self.n
    }
}
