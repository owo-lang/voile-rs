use crate::syntax::core::Val;
use crate::syntax::lisp::{self, Lisp};

fn from_str(s: &str) -> Val {
    let lisp = lisp::parse_str(s).unwrap_or_else(|err| panic!("Syntax error: `{}`.", err));
    lisp_to_term(&lisp)
}

fn lisp_to_term(lisp: &Lisp) -> Val {
    use crate::syntax::lisp::Lisp::*;
    match lisp {
        Num(dbi) => Val::var(*dbi),
        Sym(sym) => panic!("Unexpected symbol: `{}`.", sym),
        Many(block) => many_to_term(block, lisp),
    }
}

fn many_to_term(block: &[Lisp], lisp: &Lisp) -> Val {
    use crate::syntax::lisp::Lisp::*;
    match block {
        // So `()` == `()`.
        [] => Val::axiom_with_value(0),
        [Sym("fst"), arg] => lisp_to_term(arg).first(),
        [Sym("snd"), arg] => lisp_to_term(arg).second(),
        [Sym("type"), arg] => Val::Type(arg.as_dbi().unwrap() as _),
        [Sym("bot"), arg] => Val::Bot(arg.as_dbi().unwrap() as _),
        [Sym("app"), fst, snd] => lisp_to_term(fst).apply(lisp_to_term(snd)),
        [Sym("pair"), fst, snd] => Val::pair(lisp_to_term(fst), lisp_to_term(snd)),
        [Sym("lam"), fst, snd] => Val::lam(lisp_to_term(fst), lisp_to_term(snd)),
        _ => panic!("Bad block: `{}`.", lisp),
    }
}

#[test]
fn test_sanity() {
    assert_ne!(from_str("233"), from_str("()"));
    assert_ne!(from_str("(pair 114 514)"), from_str("(pair 233 666)"));
}

#[test]
fn test_parsing() {
    let _ = from_str("233");
    let _ = from_str("(type 233)");
    let _ = from_str("(bot 233)");
    let _ = from_str("(app 233 666)");
    let _ = from_str("(pair 114 514)");
    let _ = from_str("(fst 1919810)");
    let _ = from_str("(lam () 0)");
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
    assert_eq!(from_str("(app (lam () 0) 514)"), from_str("514"));
    assert_eq!(
        from_str("(app (lam () 1) 514)"),
        from_str("(app (lam () 1) 514)")
    );
}

/**
https://github.com/owo-lang/voile-rs/issues/47

First of all, `\x.\y.y` is `Lam(Lam(Dbi(0))`.
Instantiate this (e.g. with a value `114514`), according to our current strategy, will produce
`Lam(114514)`, which means that our implementation is evaluating `(\x.\y.y) 114514` to `\y.114514`.

Junk! Need to fix it asap.
*/
#[test]
fn test_issue_47() {
    assert_eq!(
        from_str("(app (lam () (lam () 0)) 114514)"),
        from_str("(lam () 0)")
    );
    assert_eq!(
        from_str("(app (lam () (lam () 1)) 114514)"),
        from_str("(lam () 114514)")
    );
    assert_eq!(
        from_str("(app (lam (lam (lam () 1) 1) 1) 514)"),
        from_str("(app (lam 514 1) 514)")
    );
}
