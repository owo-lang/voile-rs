use crate::syntax::common::DBI;
use crate::syntax::core::Term;
use crate::syntax::surf::Ident;
use std::fmt::{Display, Error as FmtError, Formatter};

/// Type-Checking Error.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TCE {
    Textual(String),
    CouldNotInfer(Term),
    TypeNotInGamma(Ident),
    /// Maximum `DBI` vs. Requested `DBI`
    DbiOverflow(DBI, DBI),
    LookUpFailed(Ident),
}

impl Display for TCE {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            TCE::Textual(text) => f.write_str(text),
            TCE::CouldNotInfer(term) => {
                use std::fmt::Debug;
                f.write_str("Could not infer type of: `")?;
                term.fmt(f)?;
                f.write_str("`.")
            }
            TCE::TypeNotInGamma(id) => {
                use std::fmt::Debug;
                f.write_str("Type info not in Gamma for: `")?;
                id.fmt(f)?;
                f.write_str("`.")
            }
            TCE::DbiOverflow(expected, actual) => {
                f.write_str("DBI overflow, maximum: ")?;
                expected.fmt(f)?;
                f.write_str(", got: ")?;
                actual.fmt(f)?;
                f.write_str(".")
            }
            TCE::LookUpFailed(var) => {
                f.write_str("Look up failed for `")?;
                // more information here?
                var.info.text.fmt(f)?;
                f.write_str("`, ")
            }
        }
    }
}
