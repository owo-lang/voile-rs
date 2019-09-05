/*!
# Voile Util

This is a crate extracted from the codebase of the [Voile] type checker
to help the development of other dependently-typed lambda calculus type checkers.

It contains helper functions for the [Pest] parser
(supported via optional feature `parser`),
a non-empty vector, some location utils, a unique-ID type with utils,
and universe level utilities (with omega).

All dependencies are optional, thus very lightweight.

 [Voile]: https://docs.rs/voile
 [Pest]: https://pest.rs
*/

/// Syntactical information.
pub mod loc;

/// Unique-ID utilities.
#[macro_use]
pub mod uid;

/// `Level`-related definitions and relevant operations and traits.
pub mod level;

/// Helper functions for pest.
#[cfg(feature = "parser")]
#[macro_use]
pub mod pest_util;

/// Not using https://docs.rs/vec1/1.4.0 because it doesn't have `fold1`.
pub mod vec1;

/// Generic meta variable utilities.
pub mod meta;
