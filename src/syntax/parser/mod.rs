/// Concrete syntax tree.
pub mod concrete;

/// Code to concrete syntax tree.
pub mod parse;

pub use self::concrete::*;
pub use self::parse::parse_str;

/// Parse a string into an optional expression and print error to stderr.
#[inline]
pub fn parse_str_err_printed(code: &str) -> Result<Vec<Declaration>, ()> {
    parse_str(code).map_err(|err| eprintln!("{}", err))
}
