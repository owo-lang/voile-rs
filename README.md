# voile-rs

[![Crates.io](https://img.shields.io/crates/d/voile.svg)][crates]
[![Crates.io](https://img.shields.io/crates/v/voile.svg)][lib-rs]
[![Crates.io](https://img.shields.io/crates/l/voile.svg)][crates]
[![docs.rs](https://docs.rs/voile/badge.svg)][doc-rs]
[![cc-svg]][cc-url]
[![av-svg]][av-url]
[![dep-svg]][dep-rs]

 [crates]: https://crates.io/crates/voile/
 [lib-rs]: https://lib.rs/voile/
 [cc-svg]: https://circleci.com/gh/owo-lang/voile-rs/tree/master.svg?style=svg
 [cc-url]: https://circleci.com/gh/owo-lang/voile-rs/tree/master
 [doc-rs]: https://docs.rs/voile
 [dep-rs]: https://deps.rs/repo/github/owo-lang/voile-rs
 [dep-svg]: https://deps.rs/repo/github/owo-lang/voile-rs/status.svg
 [plugin]: https://github.com/owo-lang/intellij-dtlc/
 [av-url]: https://ci.appveyor.com/project/ice1000/voile-rs/branch/master
 [av-svg]: https://ci.appveyor.com/api/projects/status/8rehm08ncp6whxwt/branch/master?svg=true
 [icon]: https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true
 [Narc]: https://github.com/owo-lang/narc-rs
 [minitt]: https://github.com/owo-lang/minitt-rs

Voile is a dependently-typed programming language with a non-dependent version
of row-polymorphism, meta variable resolution and implicit parameter syntax.
For language description, please head to the [docs.rs][doc-rs] page.

Voile is the language *after* [minitt],
and the *next* language after Voile is [Narc].

## Resources

![][icon]

+ [Docs.rs][doc-rs] documentation, including KaTeX-rendered typing rules
+ [Change Log](CHANGELOG.md), useful resource for tracking language evolution
+ [IntelliJ Plugin][plugin], which can export your code as clickable HTML
+ [Code Examples](samples), which also acts as integration test suites
+ [Utilities Library](voile-util), a rust crate extracted
  from Voile's implementation with some util codes
+ Windows binary download by [AppVeyor][av-url]

The most good-looking example is
[this one](samples/row-polymorphism/solve-ext-meta.voile).

## Install

You can install the voile type-checker by this command
(cargo installation and rust stable toolchain are assumed):

```bash
cargo install voile --bin voilec
```

After installation, you can type-check a voile file by:

```bash
voilec [filename]
```

You can also start a REPL:

```bash
voilec -i
```

## Progress

+ [X] Basic dependent type (minitt-rs things)
+ [X] Universe level support
+ [X] Row-types and kinds
+ [X] Record constructor
+ [X] Record projection
+ [X] Variant constructor
+ [X] Variant eliminator (case-split)
+ [X] Implicit arguments
