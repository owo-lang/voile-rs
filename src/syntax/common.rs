use pest::Span;
use std::ops::Add;

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
/// are curious but have no idea about it.
pub type DBI = usize;

pub type UID = usize;
static mut UID_COUNT: UID = 0;

pub(crate) unsafe fn next_uid() -> UID {
    let val = UID_COUNT;
    UID_COUNT += 1;
    val
}

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

impl SyntaxInfo {
    pub fn merge(self, rhs: Self, middle: &str) -> Self {
        Self {
            text: format!("({}){}({})", self.text, middle, rhs.text),
            start_line: self.start_line,
            start: self.start,
            end: rhs.end,
        }
    }
}

/// Something that holds a `SyntaxInfo`.
pub trait ToSyntaxInfo {
    /// Borrow the syntax info.
    fn syntax_info(&self) -> &SyntaxInfo;

    /// Create a cloned syntax info.
    fn to_info(&self) -> SyntaxInfo {
        self.syntax_info().clone()
    }
}

impl Add for SyntaxInfo {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.merge(rhs, "")
    }
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
