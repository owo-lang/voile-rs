mod ast;

pub use self::ast::*;

use super::common::SyntaxInfo;

impl Term {
    pub fn into_info(self, syntax_info: SyntaxInfo) -> TermInfo {
        TermInfo::new(self, syntax_info)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TermInfo {
    pub ast: Term,
    pub info: SyntaxInfo,
}

impl TermInfo {
    pub fn new(ast: Term, info: SyntaxInfo) -> Self {
        Self { ast, info }
    }

    pub fn reduce(self, arg: Term) -> Term {
        self.ast.reduce(arg)
    }

    /// Because in `reduce`, what actually moved is `self.ast`, not whole `self`.
    pub fn reduce_cloned(&self, arg: Term) -> Term {
        self.ast.clone().reduce(arg)
    }
}

#[cfg(test)]
mod tests;
