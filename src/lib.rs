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
Voile is a dependently-typed programming language evolved from [minitt][minitt].

# Design

## Goal

The focus of Voile is *extensible algebraic data types* on top of
*dependent types*.
It can solve the expression problem without using any design patterns (like
visitor or object-algebra in Java, or finally-tagless or DTALC in Haskell).

We're pretty much inspired by the coexistence of guarded recursion, coinductive
data types, sized types and inductive types in [Agda][agda],
which is nice to have all of them but they do not work very well
as we can see in a discussion [here][agda-bad-bad]
about guarded recursion checker
or in [a GitHub issue][agda-bad] about the
incompatibility between size and guarded recursion.
We can observe that sums, records, and (dependent) pattern matching in Agda only
work well when being top-level bindings.

 [agda-bad-bad]: https://github.com/agda/cubical/pull/57#discussion_r253974409
 [agda-bad]: https://github.com/agda/agda/issues/1209

## Features

+ First-class language components
  + Sum
  + Record
+ Dependent type goodies
  + Pi/Sigma

### Extensible ADTs

Voile supports sum-types (coproduct), record-types (product) as first-class
language components.

First-class record support is known as [Record Calculus][rec-calc], or
[Row Polymorphism][row-poly], or [Extensible Records][ext-rec], or "first-class
labels".
The idea of "extensible record" is that the creation, manipulation and
destruction of "records" can be done locally as an expression, without
the need of declaring a record type globally.
In other words, the "record type" itself is a kind of expression.

The study on extensible records has a long history.

However, there isn't much research and implementations on extensible sums yet.
Extensible sums are quite complicated because it probably requires:

+ First-class pattern matching
+ Subtyping on Sums

While currently, I can only find one programming language, [MLPolyR][mlpolyr]
(there's a [language spec][spec], a paper [first-class cases][fc-c],
a PhD thesis [type-safe extensible programming][tse] and an
[IntelliJ plugin][ij-dtlc]), whose pattern matching is first-class
(in the papers it's called "first-class cases").

It is quite obvious that sums and records are dual to each other -- destructing
a sum requires listing all the cases and so does constructing a record;
constructing a sum only need one element to be mentioned and so does projecting
a record element as well.
The name, *product* and *coproduct* also expresses such duality.
Duality is a very nice property to have because it implies
similar implementations.

<p style="color: yellowgreen;">
TODO This chapter is not finished.
</p>

 [rec-calc]: https://dl.acm.org/citation.cfm?id=218572
 [ext-rec]: https://wiki.haskell.org/Extensible_record
 [row-poly]: https://en.wikipedia.org/wiki/Row_polymorphism
 [mlpolyr]: https://github.com/owo-lang/mlpolyr
 [fc-c]: https://people.cs.uchicago.edu/~blume/papers/icfp06.pdf
 [tse]: https://arxiv.org/abs/0910.2654
 [spec]: https://people.cs.uchicago.edu/~blume/classes/spr2005/cmsc22620/docs/langspec.pdf
 [ij-dtlc]: https://github.com/owo-lang/intellij-dtlc

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

```ignore
Nat = 'Zero | 'Suc Nat
```

The type-checker will infinitely loop on its reduction.

The above definition will actually be rejected by the termination checker,
but recursion on sum-types *have* to be supported because we have been using
it for a long time -- we shouldn't sacrifice this fundamental language feature.

<p style="color: yellowgreen;">
TODO This chapter is not finished.
</p>

# Implementation

Voile's implementation is inspired from [Agda][agda], [mlang][mlang] and its
prototype, [minitt][minitt].

<p style="color: yellowgreen;">
TODO Something needs to be written here.
</p>

 [agda]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
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
