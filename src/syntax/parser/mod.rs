/// Surface syntax tree.
pub mod surf;

/// Code to surface syntax tree, based on pest.
pub mod parse;

pub use self::parse::parse_str;
pub use self::surf::*;

/// Parse a string into an optional expression and print error to stderr.
#[inline]
pub fn parse_str_err_printed(code: &str) -> Result<Vec<Declaration>, ()> {
    parse_str(code).map_err(|err| eprintln!("{}", err))
}
