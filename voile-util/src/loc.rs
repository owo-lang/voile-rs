use std::fmt::{Display, Error, Formatter};
use std::ops::Add;

/// Trivial information about the surface syntax items,
/// short for "Location".
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct Loc {
    pub start: usize,
    pub line: usize,
    pub end: usize,
    pub is_generated: bool,
}

/// Surface syntax tree element: Identifier.
/// Also used in other syntax trees.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub loc: Loc,
    pub text: String,
}

impl ToLoc for Ident {
    fn loc(&self) -> Loc {
        self.loc.clone()
    }
}

pub fn merge_info(a: &impl ToLoc, b: &impl ToLoc) -> Loc {
    a.loc() + b.loc()
}

/// Something that holds a `Loc`.
pub trait ToLoc {
    /// Access the location information.
    fn loc(&self) -> Loc;
}

impl Add for Loc {
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

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "line {:?} ({:?}:{:?})", self.line, self.start, self.end)
    }
}
