use std::fmt::{Display, Error, Formatter};

use crate::uid::{DBI, GI, UID};

/// Postulated value (or temporarily irreducible expressions), aka axioms.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Axiom {
    /// Functions without implementation.
    Postulated(UID),
    /// Lambda parameters during type-checking.
    /// (usually will be replaced with `Val::var` after the expression is type-checked).
    Generated(UID, DBI),
    /// Usages of definitions when they're not yet implemented.
    /// (usually will be replaced with `Val::glob` after implemented).
    Unimplemented(UID, GI),
    /// Implicit parameters during type-checking.
    Implicit(UID),
}

impl Axiom {
    pub fn unique_id(&self) -> UID {
        use Axiom::*;
        match self {
            Postulated(uid) | Generated(uid, ..) | Unimplemented(uid, ..) | Implicit(uid, ..) => {
                *uid
            }
        }
    }
}

impl Display for Axiom {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Axiom::*;
        match self {
            Postulated(uid) => write!(f, "<{}>", uid),
            Generated(uid, dbi) => write!(f, "<{} {}>", uid, dbi),
            Unimplemented(uid, dbi) => write!(f, "[|{} {}|]", uid, dbi),
            Implicit(uid) => write!(f, "{{{}}}", uid),
        }
    }
}
