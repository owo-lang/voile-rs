pub mod error;
pub mod state;

pub use self::error::*;
pub use self::state::*;
use crate::syntax::core::Term;

/// Type-Checking Monad.
pub type TCM<T = TCS> = Result<T, TCE>;

/// Term-Producing Type-Checking Monad.
pub type TermTCM = TCM<(TCS, Term)>;
