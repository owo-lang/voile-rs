/// Abstract syntax tree.
///
/// The abstract syntax tree is supposed to be representing a higher level syntax, where there
/// should be no syntactic sugars.
mod ast;

/// Desugaring the surface syntax tree to an abstract syntax tree.
mod trans;

pub use self::ast::*;
pub use self::trans::*;

#[cfg(test)]
mod tests {
    use super::{trans_decls, AbsDecl};
    use crate::syntax::surf::parse_str_err_printed;

    #[test]
    fn trans_bot() {
        let surf = parse_str_err_printed("val a : Type;\nlet a = !;").unwrap();
        let mut ctx = trans_decls(surf).unwrap();
        let decl = ctx.remove(&0).unwrap();
        match decl {
            AbsDecl::Both(_ty_info, _ty_abs, _info, _abs) => {}
            _ => panic!(),
        }
    }
}
