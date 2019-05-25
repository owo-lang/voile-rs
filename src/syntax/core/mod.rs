mod ast;
mod pretty;

pub use self::ast::*;
pub use self::pretty::*;

use super::common::{SyntaxInfo, ToSyntaxInfo, DBI};

impl Val {
    pub fn into_info(self, syntax_info: SyntaxInfo) -> ValInfo {
        ValInfo::new(self, syntax_info)
    }
}

/// A value with syntax info.
/// This is what should be stored inside of the context.
#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn increase_dbi(self, dbi: DBI) -> Self {
        self.map_ast(|ast| ast.map_neutral(|n| n.map_var(|x| x + 1 + dbi)))
    }
}

impl ToSyntaxInfo for ValInfo {
    fn syntax_info(&self) -> &SyntaxInfo {
        &self.info
    }
}

#[cfg(test)]
mod tests;
