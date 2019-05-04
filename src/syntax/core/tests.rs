use crate::syntax::core::Term;
use crate::syntax::lisp::{self, Lisp};

fn from_str(s: &str) -> Term {
    let lisp = lisp::parse_str(s).unwrap_or_else(|err| panic!("Syntax error: `{}`.", err));
    lisp_to_term(lisp)
}

fn vec_as_2<T>(v: &mut Vec<T>) -> (T, T) {
    let snd = v.pop().unwrap();
    let fst = v.pop().unwrap();
    (fst, snd)
}

fn vec_as_3<T>(v: &mut Vec<T>) -> (T, T, T) {
    let trd = v.pop().unwrap();
    let snd = v.pop().unwrap();
    let fst = v.pop().unwrap();
    (fst, snd, trd)
}

fn lisp_to_term(lisp: Lisp) -> Term {
    use crate::syntax::lisp::Lisp::*;
    match lisp {
        Num(dbi) => Term::gen(dbi),
        Sym(sym) => panic!("Unexpected symbol: `{}`.", sym),
        Many(block) => many_to_term(block),
    }
}

fn many_to_term(mut block: Vec<Lisp>) -> Term {
    use crate::syntax::lisp::Lisp::*;
    match block.len() {
        0 => Term::mock(),
        2 => match vec_as_2(&mut block) {
            (Sym(s), Num(level)) => match s.as_ref() {
                "type" => Term::Type(level as _),
                "bot" => Term::Bot(level as _),
                _ => panic!("Bad block: `{}`.", Many(block)),
            },
            _ => panic!("Bad block: `{}`.", Many(block)),
        },
        3 => match vec_as_3(&mut block) {
            (Sym(s), fst, snd) => match s.as_ref() {
                "app" => Term::app(lisp_to_term(fst).into_neutral().unwrap(), lisp_to_term(snd)),
                _ => panic!("Bad block: `{}`.", Many(block)),
            },
            _ => panic!("Bad block: `{}`.", Many(block)),
        },
        _ => panic!("Bad block: `{}`.", Many(block)),
    }
}

#[test]
fn test_parsing() {
    let _ = from_str("233");
    let _ = from_str("(type 233)");
    let _ = from_str("(bot 233)");
    let _ = from_str("(app 233 666)");
}
