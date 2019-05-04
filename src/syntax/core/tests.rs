use crate::syntax::core::Term;
use crate::syntax::lisp::{self, Lisp};

fn from_str(s: &str) -> Term {
    let lisp = lisp::parse_str(s).unwrap_or_else(|err| panic!("Syntax error: `{}`.", err));
    lisp_to_term(lisp)
}

fn lisp_to_term(lisp: Lisp) -> Term {
    use crate::syntax::lisp::Lisp::*;
    match lisp {
        Num(dbi) => Term::gen(dbi),
        Sym(sym) => panic!("Unexpected symbol: `{}`.", sym),
        Many(block) => match block.len() {
            0 => Term::mock(),
            _ => panic!("Bad block: `{}`.", Many(block)),
        },
    }
}

#[test]
fn test_parsing() {
    let _ = from_str("233");
}
