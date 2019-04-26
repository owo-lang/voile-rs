use super::{trans_decls, AbsDecl};
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
    let decl = ctx.remove(&0).unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_info, _ty_abs, _info, _abs) => {}
        _ => panic!(),
    }
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
    let decl = ctx.remove(&0).unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_info, _ty_abs, _info, _abs) => {}
        _ => panic!(),
    };
    let decl = ctx.remove(&1).unwrap();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_info, _ty_abs, _info, _abs) => {}
        _ => panic!(),
    }
}
