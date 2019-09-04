/// General definitions, `DBI`, etc.
pub mod common;

/// `Level`-related definitions, extracted from [`common`](self::common).
pub mod level;

/// A Lisp parser as some testing utility.
pub mod lisp;

/// Core language, aka well-typed terms.
pub mod core;

/// Surface syntax and the parser (based on pest).
pub mod surf;

/// Abstract syntax and the desugarer.
pub mod abs;
