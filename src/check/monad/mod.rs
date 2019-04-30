/// `Control.Monad.Except`, as type-checking error.
mod error;
/// `Control.Monad.State`, as type-checking state.
mod state;

pub use self::error::*;
pub use self::state::*;
use crate::syntax::core::TermInfo;

/// Type-Checking Monad.
pub type TCM<T = TCS> = Result<T, TCE>;

/// Term-Producing Type-Checking Monad.
pub type TermTCM = TCM<(TCS, TermInfo)>;
