# Voile's utilities

[![Crates.io](https://img.shields.io/crates/d/voile-util.svg)][crates]
[![Crates.io](https://img.shields.io/crates/v/voile-util.svg)][crates]
[![Crates.io](https://img.shields.io/crates/l/voile-util.svg)][crates]
[![docs.rs](https://docs.rs/voile-util/badge.svg)][doc-rs]

 [crates]: https://crates.io/crates/voile-util/
 [doc-rs]: https://docs.rs/voile-util

This is a crate extracted from [Voile](..)
to help the development of other
dependently-typed lambda calculus type checkers.

It contains helper functions for the [Pest] parser
(supported via optional feature `parser`),
a non-empty vector, some location utils, a unique-ID type with utils,
a lisp parser for term generation,
and universe level utilities (with omega).

 [Pest]: https://pest.rs
