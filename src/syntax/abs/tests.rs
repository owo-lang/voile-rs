use super::{trans_decls, AbsDecl};
use crate::syntax::abs::{trans_expr, Abs};
use crate::syntax::common::{DtKind, ToSyntaxInfo};
use crate::syntax::surf::parse_str_err_printed;

#[test]
fn trans_bot() {
    let surf = parse_str_err_printed(
        "val a : Type;\n\
         let a = !;",
    )
    .unwrap();
    let mut ctx = trans_decls(surf).unwrap();
    assert_eq!(1, ctx.len());
    let decl = ctx.pop().unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_abs, _abs) => {}
        _ => panic!(),
    };
}

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
    assert_eq!(2, ctx.len());
    let decl = ctx.pop().unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_abs, _abs) => {}
        _ => panic!(),
    };
    let decl = ctx.pop().unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_abs, _abs) => {}
        _ => panic!(),
    };
    assert!(ctx.is_empty());
}

fn must_be_app(abs: Abs) -> Abs {
    match abs {
        Abs::App(_, _, abs) => *abs,
        e => panic!("`{:?}` is not an `Abs::App`.", e),
    }
}

#[test]
fn trans_pi_env() {
    let pi_expr = parse_str_err_printed("val t : ((a : Type) -> (b : Type(a)) -> Type(b));")
        .unwrap()
        .remove(0)
        .body;
    match trans_expr(&pi_expr, &[], &Default::default()) {
        Ok(Abs::Dt(_, DtKind::Pi, _, box_bc)) => match *box_bc {
            Abs::Dt(_, DtKind::Pi, box_b, box_c) => {
                // the type of `b`, `c` should be _(0), _(0)
                let b = must_be_app(*box_b);
                let c = must_be_app(*box_c);
                assert_eq!(Abs::Local(b.to_info(), 0), b);
                assert_eq!(Abs::Local(c.to_info(), 0), c);
            }
            _ => panic!(),
        },
        Ok(abs) => panic!("Unexpected ok: `{:?}`", abs),
        Err(err) => panic!("Unexpected error: `{}`", err),
    }
}

#[test]
fn trans_pi_shadowing() {
    let code = "val t : ((a : Type) -> (b : Type(a)) -> (b: Type(b)) -> Type(a));";
    let pi_expr = parse_str_err_printed(code).unwrap().remove(0).body;
    let pi_abs = trans_expr(&pi_expr, &[], &Default::default()).unwrap();
    match pi_abs {
        Abs::Dt(_, DtKind::Pi, _, box_bc) => match *box_bc {
            Abs::Dt(_, DtKind::Pi, box_b1, box_bc) => {
                match *box_bc {
                    Abs::Dt(_, DtKind::Pi, box_b2, box_c) => {
                        // the type of `b1`, `b2`, `c` should be _(0), _(0), _(2)
                        let b1 = must_be_app(*box_b1);
                        let b2 = must_be_app(*box_b2);
                        let c = must_be_app(*box_c);
                        assert_eq!(Abs::Local(b1.to_info(), 0), b1);
                        assert_eq!(Abs::Local(b2.to_info(), 0), b2);
                        assert_eq!(Abs::Local(c.to_info(), 2), c);
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        },
        _ => panic!(),
    }
}
