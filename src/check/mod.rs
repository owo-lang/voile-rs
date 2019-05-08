use crate::syntax::abs::AbsDecl;

pub use self::decl::*;
pub use self::expr::*;
pub use self::util::*;

pub mod monad;

/// Declaration relevant checking.
///
/// Depends on `expr`.
mod decl;
/// Expression/Type relevant checking.
mod expr;
/// Tooling functions that depends on neither `decl` nor `expr`.
mod util;

pub fn check_main(decls: Vec<AbsDecl>) -> monad::TCM {
    check_decls(Default::default(), decls)
}
