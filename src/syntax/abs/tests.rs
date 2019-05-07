use super::{trans_decls, AbsDecl};
use crate::syntax::abs::trans_expr;
use crate::syntax::surf::parse_str_err_printed;
use std::collections::btree_map::BTreeMap;
use std::hint::unreachable_unchecked;

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
    // fixme: an assertion failed
    let pi_abs = trans_expr(&pi_expr, &[], &BTreeMap::new()).unwrap();
    print!("{:?}", pi_abs);
    unreachable!()
}
