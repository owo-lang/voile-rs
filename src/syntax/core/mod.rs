use voile_util::loc::{Loc, ToLoc};

pub use self::ast::*;
pub use self::ast_cons::*;
pub use self::level::*;
pub use self::neut_iter::*;
pub use self::pretty::*;
pub use self::redex::*;

/// Core language syntax definitions.
mod ast;
/// Constructor functions.
mod ast_cons;
/// Implementations for `Level`.
mod level;
/// Definition and implementations for `TraverseNeutral`.
mod neut_iter;
mod pretty;
/// Reduction function (red-ex stands for **red**ducible **ex**pression).
mod redex;

impl Val {
    pub fn into_info(self, syntax_info: Loc) -> ValInfo {
        ValInfo::new(self, syntax_info)
    }
}

/// A value with syntax info.
/// This is what should be stored inside of the context.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ValInfo {
    pub ast: Val,
    pub info: Loc,
}

impl ValInfo {
    pub fn new(ast: Val, info: Loc) -> Self {
        Self { ast, info }
    }

    pub fn map_ast(self, f: impl FnOnce(Val) -> Val) -> Self {
        Self::new(f(self.ast), self.info)
    }
}

impl ToLoc for ValInfo {
    fn loc(&self) -> Loc {
        self.info.clone()
    }
}

#[cfg(test)]
mod tests;
