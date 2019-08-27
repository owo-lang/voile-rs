use super::common::{SyntaxInfo, ToSyntaxInfo};

pub use self::ast::*;
pub use self::ast_cons::*;
pub use self::level::*;
pub use self::pretty::*;

mod ast;
mod ast_cons;
mod level;
mod pretty;

impl Val {
    pub fn into_info(self, syntax_info: SyntaxInfo) -> ValInfo {
        ValInfo::new(self, syntax_info)
    }
}

/// A value with syntax info.
/// This is what should be stored inside of the context.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ValInfo {
    pub ast: Val,
    pub info: SyntaxInfo,
}

impl ValInfo {
    pub fn new(ast: Val, info: SyntaxInfo) -> Self {
        Self { ast, info }
    }

    pub fn map_ast(self, f: impl FnOnce(Val) -> Val) -> Self {
        Self::new(f(self.ast), self.info)
    }
}

impl ToSyntaxInfo for ValInfo {
    fn syntax_info(&self) -> SyntaxInfo {
        self.info
    }
}

#[cfg(test)]
mod tests;
