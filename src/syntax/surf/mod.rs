pub use self::ast::*;
pub use self::parse::parse_str;

/// Surface syntax tree.
///
/// The surface syntax tree is intended to be very close to the surface syntax so it may look a bit
/// too surface (like `a|b|c` is parsed as a `[a, b, c]`-like structure instead of a
/// `[a, [b, c]]`-like structure.
mod ast;

/// Code to surface syntax tree, based on [pest](https://pest.rs).
///
/// Macro is used for code reusing.
mod parse;

/// Parse a string into an optional expression and print error to stderr.
#[inline]
pub fn parse_str_err_printed(code: &str) -> Result<Vec<Decl>, ()> {
    parse_str(code).map_err(|err| eprintln!("{}", err))
}

#[cfg(test)]
mod tests;
