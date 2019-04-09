/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DtKind {
    Pi,
    // TODO: discussion: do we need this?
    Sigma,
}

pub type Level = u32;
pub type DBI = usize;

/// Trivial information about the concrete syntax items.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SyntaxInfo {
    pub text: String,
    pub start: usize,
    pub end: usize,
}
