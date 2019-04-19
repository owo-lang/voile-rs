#![doc(
    html_logo_url = "https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true"
)]
/*!
Voile is a dependently-typed programming language evolved from [Mini-TT](https://docs.rs/minitt/).

*/

/// Abstract syntax, surface syntax, parser and well-typed terms (core language).
pub mod syntax;

/// Type-Checking module.
pub mod check;
