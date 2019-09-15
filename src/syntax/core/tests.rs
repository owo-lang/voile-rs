use voile_util::level::LiftEx;
use voile_util::lisp::{self, Lisp};
use voile_util::uid::UID;

use crate::syntax::core::Val;

fn from_str(s: &str) -> Val {
    let lisp = lisp::parse_str(s).unwrap_or_else(|err| panic!("Syntax error: `{}`.", err));
    lisp_to_val(&lisp)
}

fn lisp_to_val(lisp: &Lisp) -> Val {
    use Lisp::*;
    match lisp {
        Num(dbi) => Val::var(*dbi),
        Sym(sym) => panic!("Unexpected symbol: `{}`.", sym),
        Many(block) => many_to_val(block, lisp),
    }
}

fn many_to_val(block: &[Lisp], lisp: &Lisp) -> Val {
    use Lisp::*;
    match block {
        // So `()` == `()`.
        [] => Val::postulate(UID(0)),
        [Sym("lift"), arg] => lisp_to_val(arg).lift(1),
        [Sym("fst"), arg] => lisp_to_val(arg).first(),
        [Sym("snd"), arg] => lisp_to_val(arg).second(),
        [Sym("type"), arg] => Val::Type(From::from(arg.as_dbi().unwrap().0)),
        [Sym("app"), fst, snd] => lisp_to_val(fst).apply(lisp_to_val(snd)),
        [Sym("pair"), fst, snd] => Val::pair(lisp_to_val(fst), lisp_to_val(snd)),
        [Sym("lam"), snd] => Val::closure_lam(lisp_to_val(snd)),
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
    let _ = from_str("(app 233 666)");
    let _ = from_str("(pair 114 514)");
    let _ = from_str("(fst 1919810)");
    let _ = from_str("(lift 1919810)");
    let _ = from_str("(lam 0)");
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
    assert_eq!(&format!("{}", from_str("(snd 114514)")), "([114514].2)");
    assert_eq!(&format!("{}", from_str("(fst 114514)")), "([114514].1)");
}

#[test]
fn test_app_reduction() {
    assert_eq!(&format!("{}", from_str("(app 114 514)")), "([114] [514])");
    assert_eq!(from_str("(app (lam 0) 514)"), from_str("514"));
    assert_eq!(from_str("(app (lam 1) 514)"), from_str("1"));
}

/**
 * https://github.com/owo-lang/voile-rs/issues/47
 */
#[test]
fn test_issue_47() {
    assert_eq!(from_str("(app (lam (lam 0)) 114514)"), from_str("(lam 0)"));
    assert_eq!(
        from_str("(app (lam (lam 1)) 114514)"),
        from_str("(lam 114514)")
    );
    assert_eq!(
        from_str("(app (lam (pair (lam (pair (lam 1) 1)) 1)) 514)"),
        from_str("(pair (lam (pair (lam 1) 514)) 1)")
    );
}

#[test]
fn test_lift() {
    assert_eq!(&format!("{}", from_str("(lift 1919810)")), "[1919810]");
    assert_eq!(
        &format!("{}", from_str("(lift (lift 1919810))")),
        "[1919810]"
    );
}
