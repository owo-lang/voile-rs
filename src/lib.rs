#![doc(
    html_logo_url = "https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true"
)]
/*
Documentation guidelines

Formatting:
+ Less than 80 characters/line if possible (exceptions: for instant a link that
  is too long, where you can't cut it)
+ Use link-url separated markdown syntax
+ Colorize "TO DO"s

Phrasing:
+ Only capitalize terms when used the first time
+ Name of programming languages should be capitalized (if needed)
*/

/*!
Voile is a dependently-typed programming language evolved from [minitt].

# Design

## Goal

The focus of Voile is *extensible algebraic data types* on top of
*dependent types*.
It can solve the expression problem without using any design patterns (like
visitor or object-algebra in Java, or finally-tagless or DTALC in Haskell).

We're pretty much inspired by the coexistence of guarded recursion, coinductive
data types, sized types and inductive types in [Agda],
which is nice to have all of them but they do not work very well
as we can see in a discussion [here][agda-bad-bad]
about guarded recursion checker or in [a GitHub issue][agda-bad] about the
incompatibility between size and guarded recursion.
We can observe that sums, records, and (dependent) pattern matching in Agda only
work well when being top-level bindings.

 [agda-bad-bad]: https://github.com/agda/cubical/pull/57#discussion_r253974409
 [agda-bad]: https://github.com/agda/agda/issues/1209

## Features

+ First-class language components
  + Sum/Record
+ Dependent type goodies
  + Pi/Sigma

### Extensible ADTs

Voile supports sum-types (coproduct), record-types (product) and their
instances as first-class language components.

First-class record support is related to [Record Calculus][rec-calc], or
 [Extensible Records][ext-rec], or "first-class labels".
The idea of "extensible record" is that the creation, manipulation and
destruction of "records" can be done locally as an expression, without
the need of declaring a record type globally,
while for-all quantification should also support generalize over a part of the
records (in other words, [row-polymorphism][row-poly]).
Existing row-polymorphism implementation divides into two groups
according to how they support such generalization,
either by making "record type"/"record value" first-class expressions,
or by introducing a standalone row type.

The study on extensible records has a long history,
but there isn't much research and implementations on extensible sums
and the combination of bidirectional type-checking and row-polymorphism yet.
Extensible sums are quite complicated because it probably requires:

+ First-class pattern matching
+ Subtyping on sums

While currently, I can only find one programming language, [MLPolyR]
(there's a [language spec][spec], a paper [first-class cases][fc-c],
a PhD thesis [type-safe extensible programming][tse] and an
[IntelliJ plugin][ij-dtlc]), whose pattern matching is first-class
(in the papers it's called "first-class cases").

First-class pattern matching is useful because it solves the expression
for free, which means that library authors using such languages
can split their library features into several sub-libraries
-- one core library with many extensions. Library users can
combine the core with extensions they want like a Jigsaw
and exclude everything else (to avoid unwanted dependencies) or create their
own extensions without touching the original codebase.

 [rec-calc]: https://dl.acm.org/citation.cfm?id=218572
 [ext-rec]: https://wiki.haskell.org/Extensible_record
 [row-poly]: https://en.wikipedia.org/wiki/Row_polymorphism
 [MLPolyR]: https://github.com/owo-lang/mlpolyr
 [fc-c]: https://people.cs.uchicago.edu/~blume/papers/icfp06.pdf
 [tse]: https://arxiv.org/abs/0910.2654
 [spec]: https://people.cs.uchicago.edu/~blume/classes/spr2005/cmsc22620/docs/langspec.pdf
 [ij-dtlc]: https://github.com/owo-lang/intellij-dtlc

### Exceptions (undecided yet)

MLPolyR exploits first-class sums in error-handling -- exceptions
are treated as first-class sums while `try`-`catch` clauses are pattern
matching on them ("consumes" a variant in a sum).
Putting exceptions into function signatures
looks like Java's `checked exceptions`, but with full type-inference.
In this way, we can have cross-control-flow exceptions (instead of monadic)
safely, because it's easy to ensure that an expression is exception-free
simply by looking at its type.

We can denote function from $A$ to $B$ that may throw exception $E$ like this:

$$
A \xrightarrow{E} B
$$

A higher-order function taking an exception-throwing function and handles the
exception will have signature like this (convert langauge-level exceptions to
monadic exceptions):

$$
(A \xrightarrow{E} B) \rarr (E \rarr B) \rarr (A \rarr B)
$$

However, without the help of dependent types, there can be false positives
such as conditionally-thrown exceptions -- consider a function `f` like this
(assume $e : E$):

```mlpolyr
fun f a = if a then throw e else 0
```

Invocation `f false` is not going to raise any exception, but the compiler
disagrees because of the type of `f` inferred
(something like $\\texttt{Bool} \xrightarrow{E} \\texttt{Int}$)
is irrelevant to the argument applied.
In the industry of dependent types, there isn't much development on
exceptions. We might have a try here.

### Induction and Coinduction

According to elementary-school discrete math, induction has something
to do with recursion.
However, recursion on sum types is a huge problem against the design of
first-class sum types.

In the cliché programming languages with non-first-class sums (where the
sum types need to be declared globally before usage), type-checking against
recursive sums can be easily supported because the types are known to the
type-checker -- there's no need of reduction on an already-resolved sum type.
Once a term is known to be some sum type, it becomes a canonical value.

For Voile, arbitrary expressions can appear inside of a sum type term,
which means reduction is still needed before checking some other terms against
this sum-type-term.
If there's recursion on a sum-type-term, like the natural number definition:

```agda
Nat = 'Zero | 'Suc Nat
```

The type-checker will infinitely loop on its reduction.

The above definition will actually be rejected by the termination checker,
but recursion on sum-types *have* to be supported because we have been using
it for a long time -- we shouldn't sacrifice this fundamental language feature.

We choose to annotate types with the so-called sized types
(present in [MiniAgda]) to inform the compiler about how data size are changed
in a function.
This will also help recursive type definitions, because 

# Implementation

Voile's implementation is inspired from [Agda], [mlang], [MiniAgda] and its
prototype, [minitt].

[MiniAgda] supports induction, coinduction with sized types.

<p style="color: yellowgreen;">
TODO Something needs to be written here.
</p>

 [Agda]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
 [MiniAgda]: http://www.cse.chalmers.se/~abela/miniagda
 [mlang]: https://github.com/molikto/mlang
 [minitt]: https://lib.rs/crates/minitt

<br/>
<p>
<details>
<summary>About the name, Voile</summary>
<span>
This name is inspired from a friend whose username is <a
href="https://www.codewars.com/users/Voile"><em>Voile</em></a> (or
<a href="https://github.com/Voileexperiments"><em>Voileexperiments</em></a>).
However, this is also the name of a library in the
<a href="https://en.touhouwiki.net/wiki/Scarlet_Devil_Mansion"><em>
Scarlet Devil Mansion</em></a>.
</span></p>
<p><details>
<summary>The librarian of Voile, the Magic Library</summary>
<img src=
"https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/voile-librarian.png"
alt="" />
</details>
</details></p>
*/

/// Abstract syntax, surface syntax,
/// parser and well-typed terms (core language).
pub mod syntax;

/// Type-Checking module.
pub mod check;
