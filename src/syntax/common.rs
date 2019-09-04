use std::fmt::{Display, Error, Formatter};

use voile_util::loc::Ident;

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
