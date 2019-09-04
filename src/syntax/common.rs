use std::fmt::{Display, Error, Formatter};
use std::ops::Add;

use pest::Span;

/// Row-polymorphic types.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum VarRec {
    Variant,
    Record,
}

impl Display for VarRec {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(match self {
            VarRec::Variant => "Sum",
            VarRec::Record => "Rec",
        })
    }
}

/// Various kinds of dependent types
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum PiSig {
    Pi,
    Sigma,
}

/// Visibility of a parameter -- it can be explicit or implicit
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Plicit {
    Ex,
    Im,
}

/// Meta variable indices (they're resolved as global reference).
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct MI(pub usize);
uid_basic_operations_impl!(MI);

/// Global reference indices.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct GI(pub usize);
uid_basic_operations_impl!(GI);

impl<'a> From<Span<'a>> for SyntaxInfo {
    fn from(span: Span) -> Self {
        SyntaxInfo {
            line: span.start_pos().line_col().0,
            start: span.start(),
            end: span.end(),
            is_generated: false,
        }
    }
}

/// Trivial information about the surface syntax items.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct SyntaxInfo {
    pub start: usize,
    pub line: usize,
    pub end: usize,
    pub is_generated: bool,
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
        self.info.clone()
    }
}

pub fn merge_info(a: &impl ToSyntaxInfo, b: &impl ToSyntaxInfo) -> SyntaxInfo {
    a.syntax_info() + b.syntax_info()
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
            is_generated: self.is_generated || rhs.is_generated,
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
