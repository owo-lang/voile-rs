/// `Control.Monad.Except`, as type-checking error.
mod error;
/// `Control.Monad.State`, as type-checking state.
mod state;

pub use self::error::*;
pub use self::state::*;
use crate::syntax::core::ValInfo;

/// Type-Checking Monad.
pub type TCM<T = TCS> = Result<T, TCE>;

/// Val-Producing Type-Checking Monad.
pub type ValTCM = TCM<(ValInfo, TCS)>;
