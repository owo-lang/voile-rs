/// AST, CST, well-typed term.
pub mod syntax;

/// Type-Checking module.
pub mod check;

#[cfg(feature = "cli")]
pub mod cli;
