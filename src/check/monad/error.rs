use crate::syntax::core::Term;
use core::fmt::Debug;
use std::fmt::{Display, Error as FmtError, Formatter};

/// Type-Checking Error.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TCE {
    Textual(String),
    CouldNotInfer(Term),
    NotImplemented,
}

impl Display for TCE {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            TCE::Textual(text) => f.write_str(text),
            TCE::NotImplemented => f.write_str("Not implemented yet."),
            TCE::CouldNotInfer(term) => {
                f.write_str("Could not infer type of: `")?;
                term.fmt(f)?;
                f.write_str("`.")
            }
        }
    }
}
