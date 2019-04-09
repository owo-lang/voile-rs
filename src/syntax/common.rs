use pest::Span;

/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DtKind {
    Pi,
    // TODO: discussion: do we need this?
    Sigma,
}

pub type Level = u32;
pub type DBI = usize;

impl<'a> From<Span<'a>> for SyntaxInfo {
    fn from(span: Span) -> Self {
        SyntaxInfo {
            text: span.as_str().to_string(),
            start: span.start(),
            end: span.end(),
        }
    }
}

/// Trivial information about the surface syntax items.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
pub struct SyntaxInfo {
    pub text: String,
    pub start: usize,
    pub end: usize,
}
