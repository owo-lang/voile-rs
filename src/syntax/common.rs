use pest::Span;

/// Indicates that whether a parameter is implicit or explicit.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum ParamKind {
    Explicit,
    Implicit,
}

/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum DtKind {
    Pi,
    // TODO: discussion: do we need this?
    Sigma,
}

/// Level literals (the `233` part of `Type233` literals)
pub type Level = u32;

/// De Bruijn Indices. Checkout [Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index) if you
/// have no idea about this and you are curious about it.
pub type DBI = usize;

impl<'a> From<Span<'a>> for SyntaxInfo {
    fn from(span: Span) -> Self {
        SyntaxInfo {
            text: span.as_str().to_string(),
            start_line: span.start_pos().line_col().0,
            start: span.start(),
            end: span.end(),
        }
    }
}

/// Trivial information about the surface syntax items.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
pub struct SyntaxInfo {
    /// This is used for pretty-printing, it may probably be different from the original text.
    pub text: String,
    pub start: usize,
    pub start_line: usize,
    pub end: usize,
}

impl std::fmt::Display for SyntaxInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "expression `{}` at line {:?} ({:?}:{:?})",
            self.text, self.start_line, self.start, self.end
        )
    }
}
