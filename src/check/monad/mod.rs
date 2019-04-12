pub mod error;
pub mod state;

pub use self::error::*;
pub use self::state::*;

/// Type-Checking Monad.
pub type TCM<T = TCS> = Result<T, TCE>;
