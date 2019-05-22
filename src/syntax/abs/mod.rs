/// Abstract syntax tree.
///
/// The abstract syntax tree is supposed to be representing a higher level syntax, where there
/// should be no syntactic sugars.
mod ast;

/// Desugaring the surface syntax tree to an abstract syntax tree.
mod trans;

/// Pretty-print AST.
mod pretty;

pub use self::ast::*;
pub use self::pretty::*;
pub use self::trans::*;

#[cfg(test)]
mod tests;
