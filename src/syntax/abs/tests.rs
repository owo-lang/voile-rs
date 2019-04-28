use super::{trans_decls, AbsDecl};
use crate::syntax::surf::parse_str_err_printed;

#[test]
fn trans_bot() {
    let surf = parse_str_err_printed(
        "val a : Type;\n\
         let a = !;",
    )
    .unwrap();
    let ctx = trans_decls(surf).unwrap();
    assert_eq!(1, ctx.len());
    let decl = ctx[0].clone();
    println!("{:?}", decl);
    match decl {
        AbsDecl::Both(_ty_info, _ty_abs, _info, _abs) => {}
        _ => panic!(),
    }
}
