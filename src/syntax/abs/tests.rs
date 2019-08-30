use crate::check::monad::TCE;
use crate::syntax::abs::{trans_expr, Abs};
use crate::syntax::common::{Ident, PiSig, DBI, GI, MI};
use crate::syntax::surf::parse_str_err_printed;

use super::{trans_decls, AbsDecl};

#[test]
fn many_decls() {
    let surf = parse_str_err_printed(
        "val a : Type1;\n\
         let a = Type;\n\
         val b : Type1;\n\
         let b = Type;",
    )
    .unwrap();
    let mut ctx = trans_decls(surf).unwrap();
    assert_eq!(4, ctx.len());
    let decl = ctx.pop().unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Impl(abs, ty_dbi) => {
            println!("val {};", ty_dbi);
            println!("let {};", abs);
        }
        _ => panic!(),
    };
    ctx.pop().unwrap();
    let decl = ctx.pop().unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Impl(abs, ty_dbi) => {
            println!("val {};", ty_dbi);
            println!("let {};", abs);
        }
        _ => panic!(),
    };
    ctx.pop().unwrap();
    assert!(ctx.is_empty());
}

fn must_be_app(abs: Abs) -> Abs {
    match abs {
        Abs::App(_, _, _, abs) => *abs,
        e => panic!("`{:?}` is not an `Abs::App`.", e),
    }
}

fn must_be_pi(abs: Abs) -> (Abs, Abs) {
    match abs {
        Abs::Dt(_, PiSig::Pi, _, _, param, abs) => (*param, *abs),
        e => panic!("`{:?}` is not an `Abs::Dt(_, Pi, _, _)`.", e),
    }
}

fn must_be_lam(abs: Abs) -> Abs {
    match abs {
        Abs::Lam(_, _, _, abs) => *abs,
        e => panic!("`{:?}` is not an `Abs::Lam(_, _, _)`.", e),
    }
}

fn must_be_local(abs: Abs) -> DBI {
    match abs {
        Abs::Var(_, _, dbi) => dbi,
        e => panic!("`{:?}` is not an `Abs::Local(_, _, _)`.", e),
    }
}

#[test]
fn trans_pi_env() {
    let pi_expr = parse_str_err_printed("val t : ((a : Type) -> (b : Type(a)) -> Type(b));")
        .unwrap()
        .remove(0)
        .body;
    let pi_expr = trans_expr(pi_expr, &[], &mut Default::default(), &Default::default())
        .expect("Parse failed.");
    println!("{}", pi_expr);
    let (_, bc) = must_be_pi(pi_expr);
    let (b, c) = must_be_pi(bc);
    // the type of `b`, `c` should be _(0), _(0)
    let b = must_be_app(b);
    let c = must_be_app(c);
    assert_eq!(DBI(0), must_be_local(b));
    assert_eq!(DBI(0), must_be_local(c));
}

#[test]
fn trans_pi_shadowing() {
    let code = "val t : ((a : Type) -> (b : Type(a)) -> (b: Type(b)) -> Type(a));";
    let pi_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let pi_abs = trans_expr(pi_expr, &[], &mut Default::default(), &Default::default()).unwrap();
    println!("{}", pi_abs);
    let (_, bc) = must_be_pi(pi_abs);
    let (b1, bc) = must_be_pi(bc);
    let (b2, c) = must_be_pi(bc);
    // the type of `b1`, `b2`, `c` should be _(0), _(0), _(2)
    let b1 = must_be_app(b1);
    let b2 = must_be_app(b2);
    let c = must_be_app(c);
    assert_eq!(DBI(0), must_be_local(b1));
    assert_eq!(DBI(0), must_be_local(b2));
    assert_eq!(DBI(2), must_be_local(c));
}

#[test]
fn trans_lam() {
    let code = r"let l = \a . \b . \a . b a;";
    let lam_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let lam_abs = trans_expr(lam_expr, &[], &mut Default::default(), &Default::default()).unwrap();
    println!("{}", lam_abs);
    let abs_lam_ba = must_be_lam(lam_abs);
    let abs_lam_a = must_be_lam(abs_lam_ba);
    match must_be_lam(abs_lam_a) {
        Abs::App(_, b, _, a) => {
            // lam body should be App(_info, Local(_info, 1), Local(_info, 0))
            assert_eq!(DBI(1), must_be_local(*b));
            assert_eq!(DBI(0), must_be_local(*a));
        }
        _ => panic!(),
    }
}

#[test]
fn trans_multi_param_lam() {
    let code = r"let l = \a b a . b a;";
    let lam_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let lam_abs = trans_expr(lam_expr, &[], &mut Default::default(), &Default::default()).unwrap();
    println!("{}", lam_abs);
    let abs_lam_ba = must_be_lam(lam_abs);
    let abs_lam_a = must_be_lam(abs_lam_ba);
    match must_be_lam(abs_lam_a) {
        Abs::App(_, b, _, a) => {
            // lam body should be App(_info, Local(_info, 1), Local(_info, 0))
            assert_eq!(DBI(1), must_be_local(*b));
            assert_eq!(DBI(0), must_be_local(*a));
        }
        _ => panic!(),
    }
}

#[test]
fn trans_lam_lookup_failed() {
    let code = r"let l = \a . b;";
    let lam_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let tce = trans_expr(lam_expr, &[], &mut Default::default(), &Default::default()).unwrap_err();
    match tce {
        TCE::LookUpFailed(ident) => assert_eq!(ident.text, "b"),
        _ => panic!(),
    }
}

#[test]
fn trans_lam_global() {
    let code = r"let l = \a . b;";
    let lam_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let ident = Ident {
        text: "".to_owned(),
        info: Default::default(),
    };
    let lam_abs = trans_expr(
        lam_expr,
        &[AbsDecl::Decl(Abs::Meta(ident, MI(0)))],
        &mut MI(1),
        &[("b".to_string(), GI(0))].iter().cloned().collect(),
    )
    .unwrap();
    println!("{}", lam_abs);
    match lam_abs {
        Abs::Lam(_, _, _, global_b) => match *global_b {
            Abs::Ref(_, b_index) => assert_eq!(b_index, GI(0)),
            _ => panic!(),
        },
        _ => panic!(),
    }
}
