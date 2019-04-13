use crate::syntax::core::Term;
use crate::syntax::surf::ast::Ident;
use core::fmt::Debug;
use std::fmt::{Display, Error as FmtError, Formatter};

/// Type-Checking Error.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TCE {
    Textual(String),
    CouldNotInfer(Term),
    TypeNotInGamma(Ident),
    DbiOverflow,
}

impl Display for TCE {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            TCE::Textual(text) => f.write_str(text),
            TCE::CouldNotInfer(term) => {
                f.write_str("Could not infer type of: `")?;
                term.fmt(f)?;
                f.write_str("`.")
            }
            TCE::TypeNotInGamma(id) => {
                f.write_str("Type info not in Gamma for: `")?;
                id.fmt(f)?;
                f.write_str("`.")
            }
            TCE::DbiOverflow => f.write_str("DBI overflow."),
        }
    }
}
