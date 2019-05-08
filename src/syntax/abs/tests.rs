use super::{trans_decls, AbsDecl};
use crate::syntax::abs::{trans_expr, Abs};
use crate::syntax::common::DtKind;
use crate::syntax::surf::parse_str_err_printed;
use std::collections::btree_map::BTreeMap;

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

#[test]
fn trans_pi_env() {
    let pi_expr = parse_str_err_printed("val t : ((a : Type) -> (b : Type(a)) -> Type(b));")
        .unwrap()[0]
        .clone()
        .body;
    let pi_abs = trans_expr(&pi_expr, &[], &BTreeMap::new()).unwrap();
    match pi_abs {
        Abs::Dt(_, DtKind::Pi, _, box_bc) => match *box_bc {
            Abs::Dt(_, DtKind::Pi, box_b, box_c) => {
                // the type of `b`, `c` should be _(0), _(0)
                match (*box_b, *box_c) {
                    (Abs::App(_, _, b), Abs::App(_, _, c)) => match (*b, *c) {
                        (Abs::Local(_, 0), Abs::Local(_, 0)) => {}
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
            _ => panic!(),
        },
        _ => panic!(),
    }
}
