mod ast;

pub use self::ast::*;

use super::common::SyntaxInfo;

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
}

#[cfg(test)]
mod tests;
