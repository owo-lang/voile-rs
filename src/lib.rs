#![doc(
    html_logo_url = "https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true"
)]
/*!
Voile is a dependently-typed programming language evolved from [Mini-TT][mtt].

 [mtt]: https://docs.rs/minitt/

# Design

### Goal

The focus of Voile is *extensible algebraic data types* on top of
*dependent types*.
It can solve the expression problem without using any design patterns (like
visitor or object-algebra in Java, or finally-tagless or DTALC in Haskell).

We're pretty much inspired by the coexistence of guarded recursion, coinductive
data types, and inductive types in Agda,
which does not work very well according to a discussion [here][agda-bad-bad].
We can observe that Sums, Records, and (Dependent) Pattern matching in Agda only
work well when being top-level bindings.

 [agda-bad-bad]: https://github.com/agda/cubical/pull/57#discussion_r253974409

### Features

Voile is supporting Sum-types (coproduct), Record-types (product) as first-class
language components.

First-class record support is known as [Record Calculus][rec-calc], or
[Row Polymorphism][row-poly], or [Extensible Records][ext-rec], or "first-class
labels".
The idea of "Extensible Record" is that the creation, manipulation and
destruction of "Records" can be done locally -- without the need of declaring a
record type globally.
In other words, the "Record type" itself is a kind of expression.

The study on extensible records has a long history.

<p style="color: yellowgreen;">
TODO Something needs to be written here.
</p>

However, there isn't much research and implementations on extensible Sums yet.
Extensible Sums are quite complicated because it probably requires:

+ First-class pattern matching
+ Subtyping on Sums

While currently, I can only find one programming language, [MLPolyR][mlpolyr]
([language spec][spec], papers: [first-class cases][fc-c],
[type-safe extensible programming][tse]), whose pattern matching is first-class
(in the papers it's called "first-class cases").

 [rec-calc]: https://dl.acm.org/citation.cfm?id=218572
 [ext-rec]: https://wiki.haskell.org/Extensible_record
 [row-poly]: https://en.wikipedia.org/wiki/Row_polymorphism
 [mlpolyr]: https://github.com/nojb/mlpolyr
 [fc-c]: https://people.cs.uchicago.edu/~blume/papers/icfp06.pdf
 [tse]: https://arxiv.org/abs/0910.2654
 [spec]: https://people.cs.uchicago.edu/~blume/classes/spr2005/cmsc22620/docs/langspec.pdf

# Implementation

Voile's implementation is inspired from [Agda][agda], [mlang][mlang] and its
prototype, [minitt][minitt].

<p style="color: yellowgreen;">
TODO Something needs to be written here.
</p>

 [agda]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
 [mlang]: https://github.com/molikto/mlang
 [minitt]: https://lib.rs/crates/minitt

## Bottlenecks

Recursion on sum types is a huge problem against the design of first-class
sum types.

In the cliché programming languages with non-first-class Sums (where the
sum types need to be declared globally before usage), type-checking against
recursive Sums can be easily supported because the types are known to the
type-checker -- there's no need of reduction on an already-resolved sum type.
Once a term is known to be some sum type, it becomes a canonical value.

For Voile, arbitrary expressions can appear inside of a sum type term,
which means reduction is still needed before checking some other terms against
this sum-type-term.
If there's recursion on a sum-type-term, like the Natural number definition:

```ignore
Nat = 'Zero | 'Suc Nat
```

The type-checker will infinitely loop on its reduction.

The above definition will actually be rejected by the termination checker,
but recursion on Sum-types *have* to be supported because we have been using
it for a long time -- we shouldn't sacrifice this fundamental language feature.

<p style="color: yellowgreen;">
TODO Something needs to be written here.
</p>

<br/>
<details>
<summary>About the name, Voile</summary>
<span>
This name is inspired from a friend whose username is [*Voile*][vo] (or [*Voileexperiments*][ve]).
However, this is also the name of a library in the [*Scarlet Devil Mansion*][sdm].
</span>

<details>
<summary>The librarian of Voile, the Magic Library</summary>
![](https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/voile-librarian.png)
</details>

 [vo]: https://www.codewars.com/users/Voile
 [ve]: https://github.com/Voileexperiments
 [sdm]: https://en.touhouwiki.net/wiki/Scarlet_Devil_Mansion
</details>
*/

/// Abstract syntax, surface syntax, parser and well-typed terms (core language).
pub mod syntax;

/// Type-Checking module.
pub mod check;
