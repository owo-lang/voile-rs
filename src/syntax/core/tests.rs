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
            (Sym(s), arg) => match s.as_ref() {
                "fst" => lisp_to_term(arg).first(),
                "snd" => lisp_to_term(arg).second(),
                "type" => Term::Type(arg.into_dbi().unwrap() as _),
                "bot" => Term::Bot(arg.into_dbi().unwrap() as _),
                _ => panic!("Bad block: `{}`.", Many(block)),
            },
            _ => panic!("Bad block: `{}`.", Many(block)),
        },
        3 => match vec_as_3(&mut block) {
            (Sym(s), fst, snd) => match s.as_ref() {
                "app" => lisp_to_term(fst).apply(lisp_to_term(snd)),
                "pair" => Term::pair(lisp_to_term(fst), lisp_to_term(snd)),
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
    let _ = from_str("(pair 114 514)");
    let _ = from_str("(fst 1919810)");
}

#[test]
fn test_pair_reduction() {
    assert_eq!(from_str("(fst (pair 114 514))"), from_str("114"));
    assert_eq!(from_str("(snd (pair 114 514))"), from_str("514"));
    assert_eq!(
        from_str("(snd (pair 114 (type 514)))"),
        from_str("(type 514)")
    );
    assert_eq!(from_str("(snd 114514)"), from_str("(snd 114514)"));
}
