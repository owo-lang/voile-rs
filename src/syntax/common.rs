use std::fmt::{Display, Error, Formatter};
use std::ops::{Add, AddAssign};

use pest::Span;

/// Row-polymorphic types.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum VarRec {
    Variant,
    Record,
}

/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum PiSig {
    Pi,
    Sigma,
}

macro_rules! impl_usize {
    ($name:ident) => {
        impl Add<usize> for $name {
            type Output = $name;

            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }

        impl Add for $name {
            type Output = $name;

            fn add(self, rhs: $name) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }

        impl AddAssign<usize> for $name {
            fn add_assign(&mut self, rhs: usize) {
                self.0 += rhs
            }
        }

        impl AddAssign for $name {
            fn add_assign(&mut self, rhs: $name) {
                self.0 += rhs.0
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                self.0.fmt(f)
            }
        }
    };
}

/// De Bruijn Indices. Checkout [Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index) if you
/// are curious but have no idea about it.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct DBI(pub usize);
impl_usize!(DBI);

/// Meta variable indices (they're resolved as global reference).
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct MI(pub usize);
impl_usize!(MI);

/// Global reference indices.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct GI(pub usize);
impl_usize!(GI);

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct UID(pub usize);
impl_usize!(UID);

static mut UID_COUNT: usize = 0;

pub(crate) unsafe fn next_uid() -> UID {
    let val = UID_COUNT;
    UID_COUNT += 1;
    UID(val)
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

impl Display for SyntaxInfo {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "line {:?} ({:?}:{:?})", self.line, self.start, self.end)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
/// Typed label
pub struct Labelled<Expr> {
    /// Label is an identifier
    pub label: Ident,
    /// The thing attached on this label
    pub expr: Expr,
}

impl<Expr> Labelled<Expr> {
    pub fn map_expr<Abs>(self, f: impl FnOnce(Expr) -> Abs) -> Labelled<Abs> {
        Labelled {
            label: self.label,
            expr: f(self.expr),
        }
    }
}
