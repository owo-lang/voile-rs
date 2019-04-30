use crate::syntax::abs::AbsDecl;

pub use self::decl::*;
pub use self::expr::*;

pub mod monad;

/// Declaration relevant checking.
///
/// Depends on `expr`.
mod decl;
/// Expression/Type relevant checking.
mod expr;

pub fn check_main(decls: Vec<AbsDecl>) -> self::monad::TCM {
    check_declarations(Default::default(), decls)
}
