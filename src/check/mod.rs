use crate::syntax::abs::AbsDecl;

pub use self::decl::*;
pub use self::eval::*;
pub use self::expr::*;

pub mod monad;

/// Declaration relevant checking.
///
/// Depends on `expr`.
mod decl;
/// Tooling functions that depends on neither `decl` nor `expr`.
mod eval;
/// Expression/Type relevant checking.
mod expr;

pub fn check_main(decls: Vec<AbsDecl>) -> monad::TCM {
    check_decls(Default::default(), decls)
}
