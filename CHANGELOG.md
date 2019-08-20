# Change Log

# 0.1.1

+ Update typing rule for `cons`

# 0.1.0

+ Fix core language `is_type` judgement
+ Update mod rustdoc
+ Evaluation for `RowPoly`

# 0.0.15

+ Error message is now "change my mind" (#126)
+ Introduce `vec1` (#127)
+ New syntax for `Rec` and `Sum`
+ Remove `check_type(e)`, replace with `check(e, TypeOmega)` (#140)
+ Add subtyping back (#132)
+ Add row kinds according to the paper (#83)
+ Row-kinding type-checking (#137)
+ Update KaTeX and crate rustdoc

# 0.0.14

+ Documentation cleanup
+ Report error on redefinitions (#107)
+ Unification (#105, #110)
+ Meta variable solving (#104, #110, #119, #120)
+ Use Rust's newtype pattern to represent different indices (#111, #112)
+ Infer lambda types using metas (#117)
+ Memory optimizations
+ Update dependency versions

# 0.0.9

+ Remove `Bot` in core (#78)
+ Introduce standalone `Axiom` type (#91)
+ Code cleanup, remove `compile` and `Name` (#84)
+ Introduce level lifting operation and level checking (#77)
+ Remove lambda parameter (#89)
+ Rename `Abs::Var` and `Abs::Local` to `Abs::Ref` and `Abs::Var`
  respectively to fit `Val`'s naming convention
+ Redesigned global references (#96) to support mutual recursion
+ Introduce omega level (#99)
+ Support inferring omega level (#101). This does not look like
  something that will be used in the near future.
+ Add `-e` option for `voilec`,
  add `:infer`, `:eval`, and `:level` REPL commands (#90)
+ Use more TeX in `lib.rs` comments

# 0.0.8

+ Rename `ConsType` to `Variant`, `Term` to `Val`
+ Improve lambda compilation (#71, #73)
+ `Variant` related checking, introduce `Sum` in core (#11, #70)
+ Remove features of `clap` to avoid transitive winapi
  0.2.8 dependency
+ Now we're not compiling after tycking, but transform the
  tycked terms (#74, #75)
+ REPL (#56, #76)
+ The message "Type-Check successful." is changed to
  "Checkmate, dram!"
+ Fix wrong desugar of sum expression
+ Fix closure instantiation

# 0.0.7

+ Tuple desugar (#51)
+ Lambda desugar (#59, #62)
+ Upgrade KaTeX to 0.10.2
+ Largely refactor abstract syntax translation logic
+ Abstract/Core pretty-printer
+ Fix evaluator and parameter desugarer (#44, #67, #68)

# 0.0.6

+ Give up implicit parameters (#32)
+ Cleanup (#23, #39, #41)
+ Improve abstract syntax (#21, #24, #43)
+ Add documentation guidelines
+ Improve type-checking state (#15, #39, #40)
+ Fix DBI-based lambda instantiation for real (#47, #48)
+ Implement shadowing (#45)
+ Redesign lambda syntax (#46, #49)

# 0.0.5

+ Introduce core term tests (#17)
+ Fix DBI-based lambda instantiation (#16)
+ Remove captured env in closure (#19)
+ Introduce `RedEx`

# 0.0.4

+ Introduce Type-Checking Monad
+ Introduce the abstract syntax
+ Basic desugar: translate surface syntax to abstract syntax
+ Improve documentation, start writing the tutorial

# 0.0.3

+ Implemented the expression parser
+ Replace `Pi`, `Sigma` with `Dt`
+ Further improve the surface syntax tree for expressions

# 0.0.2

+ Implemented a STLC core language
+ Parser prototype
+ CLI prototype

# 0.0.1

+ Create package on crates.io
