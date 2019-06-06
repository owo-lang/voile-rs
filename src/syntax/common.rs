use pest::Span;
use std::ops::Add;

/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum DtKind {
    Pi,
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
            line: span.start_pos().line_col().0,
            start: span.start(),
            end: span.end(),
        }
    }
}

/// Trivial information about the surface syntax items.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct SyntaxInfo {
    pub start: usize,
    pub line: usize,
    pub end: usize,
}

/// Surface syntax tree element: Identifier.
/// Also used in other syntax trees.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub info: SyntaxInfo,
    pub text: String,
}

impl ToSyntaxInfo for Ident {
    fn syntax_info(&self) -> SyntaxInfo {
        self.info
    }
}

/// Something that holds a `SyntaxInfo`.
pub trait ToSyntaxInfo {
    /// Borrow the syntax info.
    fn syntax_info(&self) -> SyntaxInfo;
}

impl Add for SyntaxInfo {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            line: self.line,
            start: self.start,
            end: rhs.end,
        }
    }
}

impl std::fmt::Display for SyntaxInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "line {:?} ({:?}:{:?})", self.line, self.start, self.end)
    }
}
