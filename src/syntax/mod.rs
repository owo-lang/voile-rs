/// General definitions, like `Level`, `DBI`, etc.
pub mod common;

/// Environment, context, de Bruijn indices
pub mod env;

/// Helper functions for pest.
#[macro_use]
pub mod pest_util;

/// Core language, aka well-typed terms.
pub mod core;

/// Surface syntax and the parser (based on pest).
pub mod surf;

/// Abstract syntax and the desugarer.
pub mod abs;
