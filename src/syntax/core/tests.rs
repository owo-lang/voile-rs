use crate::syntax::core::Term;
use crate::syntax::lisp::{self, Lisp};

fn from_str(s: &str) -> Term {
    let lisp = lisp::parse_str(s).unwrap_or_else(|err| panic!("Syntax error: `{}`.", err));
    lisp_to_term(&lisp)
}

fn lisp_to_term(lisp: &Lisp) -> Term {
    use crate::syntax::lisp::Lisp::*;
    match lisp {
        Num(dbi) => Term::var(*dbi),
        Sym(sym) => panic!("Unexpected symbol: `{}`.", sym),
        Many(block) => many_to_term(block, lisp),
    }
}

fn many_to_term(block: &[Lisp], lisp: &Lisp) -> Term {
    use crate::syntax::lisp::Lisp::*;
    match block {
        [] => Term::axiom(),
        [Sym(s), arg] => match *s {
            "fst" => lisp_to_term(arg).first(),
            "snd" => lisp_to_term(arg).second(),
            "type" => Term::Type(arg.as_dbi().unwrap() as _),
            "bot" => Term::Bot(arg.as_dbi().unwrap() as _),
            _ => panic!("Bad block: `{}`.", lisp),
        },
        [Sym(s), fst, snd] => match *s {
            "app" => lisp_to_term(fst).apply(lisp_to_term(snd)),
            "pair" => Term::pair(lisp_to_term(fst), lisp_to_term(snd)),
            _ => panic!("Bad block: `{}`.", lisp),
        },
        _ => panic!("Bad block: `{}`.", lisp),
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
    assert_eq!(
        from_str("(snd (fst (pair (pair 114 (type 514)) ())))"),
        from_str("(type 514)")
    );
    assert_eq!(from_str("(snd 114514)"), from_str("(snd 114514)"));
    assert_eq!(from_str("(fst 114514)"), from_str("(fst 114514)"));
}

#[test]
fn test_app_reduction() {
    assert_eq!(from_str("(app 114 514)"), from_str("(app 114 514)"));
}
