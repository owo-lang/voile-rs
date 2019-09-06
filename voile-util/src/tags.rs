use std::fmt::{Display, Error, Formatter};

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

impl Display for PiSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(match self {
            PiSig::Pi => "\u{03A0}",
            PiSig::Sigma => "\u{03A3}",
        })
    }
}

/// Visibility of a parameter -- it can be explicit or implicit
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Plicit {
    Ex,
    Im,
}
