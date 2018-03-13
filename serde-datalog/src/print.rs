use std::fmt::{Display, Formatter, Result as FmtResult};

use ast_builder::Ast;

impl Display for Ast {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let mut first = true;
        for (functor, terms) in self.0.iter() {
            if first {
                first = false;
            } else {
                writeln!(fmt, "")?;
            }
            sparkly_facts(functor, terms, fmt)?;
        }
        Ok(())
    }
}

fn printable(ch: char) -> bool {
    let ch = ch as u32;
    ch >= 0x20 && ch < 0x7f
}

fn symbol_char(ch: char) -> bool {
    match ch {
        '_' => true,
        '0'...'9' => true,
        'a'...'z' => true,
        'A'...'Z' => true,
        _ => false,
    }
}

fn symbol_start_char(ch: char) -> bool {
    match ch {
        '0'...'9' => true,
        'a'...'z' => true,
        _ => false,
    }
}

fn prolog_char(ch: char, fmt: &mut Formatter) -> FmtResult {
    let n = ch as u32;
    if ch == '\'' || ch == '\\' {
        write!(fmt, "\\{}", ch)
    } else if printable(ch) {
        write!(fmt, "{}", ch)
    } else if n <= 0xffff {
        write!(fmt, "\\u{:04x}", n)
    } else {
        write!(fmt, "\\U{:08x}", n)
    }
}

fn sparkly_term(term: &str, fmt: &mut Formatter) -> FmtResult {
    let needs_quotes = term == "" || term.contains(|ch| !symbol_char(ch))
        || term.chars()
            .next()
            .map(|ch| !symbol_start_char(ch))
            .unwrap_or(false);
    if needs_quotes {
        write!(fmt, "'")?;
        for ch in term.chars() {
            prolog_char(ch, fmt)?;
        }
        write!(fmt, "'")
    } else {
        write!(fmt, "{}", term)
    }
}

fn sparkly_fact(
    functor: &str,
    terms: &[String],
    fmt: &mut Formatter,
) -> FmtResult {
    sparkly_term(functor, fmt)?;
    write!(fmt, "(")?;
    let mut first = true;
    for term in terms {
        if first {
            first = false;
        } else {
            write!(fmt, ", ")?;
        }
        sparkly_term(term, fmt)?;
    }
    writeln!(fmt, ").")
}

fn sparkly_facts(
    functor: &str,
    facts: &[Vec<String>],
    fmt: &mut Formatter,
) -> FmtResult {
    let mut indices = (0..facts.len()).collect::<Vec<_>>();
    indices.sort_by_key(|&i| facts[i].len());

    for i in indices {
        let terms = &facts[i];
        sparkly_fact(functor, terms, fmt)?;
    }
    Ok(())
}
