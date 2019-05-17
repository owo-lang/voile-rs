mod ast;

pub use self::ast::*;

use super::common::{SyntaxInfo, ToSyntaxInfo};

impl Term {
    pub fn into_info(self, syntax_info: SyntaxInfo) -> TermInfo {
        TermInfo::new(self, syntax_info)
    }
}

/// A term with syntax info.
/// This is what should be stored inside of the context.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TermInfo {
    pub ast: Term,
    pub info: SyntaxInfo,
}

impl TermInfo {
    pub fn new(ast: Term, info: SyntaxInfo) -> Self {
        Self { ast, info }
    }

    pub fn map_ast(self, f: impl FnOnce(Term) -> Term) -> Self {
        Self::new(f(self.ast), self.info)
    }

    pub fn increase_dbi(self, dbi: DBI) -> Self {
        self.map_ast(|ast| ast.map_neutral(|n| n.map_var(|x| x + 1 + dbi)))
    }
}

impl ToSyntaxInfo for TermInfo {
    fn syntax_info(&self) -> &SyntaxInfo {
        &self.info
    }
}

#[cfg(test)]
mod tests;
