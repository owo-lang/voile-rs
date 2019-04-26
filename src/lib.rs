#![doc(
    html_logo_url = "https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true"
)]
/*!
Voile is a dependently-typed programming language evolved from [Mini-TT][mtt].

 [mtt]: https://docs.rs/minitt/

# Design

### Goal

The focus of Voile is *extensible algebraic data types*.
It can solve the expression problem without using any design patterns (like
visitor or object-algebra in Java, or finally-tagless or dtlac in Haskell).

### Features

Voile is supporting sum-types (coproduct), record types (product) as first-class
language components.

First-class record support is known as [Record Calculus][rec-calc], or
[Row Polymorphism][row-poly], or [Extensible Records][ext-rec], or "first-class
labels".
The idea of "extensible record" is that the creation, manipulation and
destruction of "records" can be done locally -- without the need of declaring a
record type globally.
In other words, the "record type" itself is a kind of expression.

The study on type systems against extensible records has a long history.

<p style="color: yellowgreen;">
TODO Something need to be written here.
</p>

However, there lacks research and implementations on extensible sums.

 [rec-calc]: https://dl.acm.org/citation.cfm?id=218572
 [ext-rec]: https://wiki.haskell.org/Extensible_record
 [row-poly]: https://en.wikipedia.org/wiki/Row_polymorphism

# Implementation

Voile's implementation is inspired from [Agda][agda], [mlang][mlang] and its
prototype, [minitt][minitt].

<p style="color: yellowgreen;">
TODO Something need to be written here.
</p>

 [agda]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
 [mlang]: https://github.com/molikto/mlang
 [minitt]: https://lib.rs/crates/minitt

## Bottlenecks

Recursion on sum types is a very big problem against the design of first-class
sum types.

In the cliché programming languages supporting non-first-class sums (where the
sum types need to be declared globally before usage), type-checking against
recursive sums can be easily supported because the types are known to the
type-checker -- there's no need of reduction on an already-resolved sum type.
Once a term is known to be some sum type, it becomes a canonical value.

For Voile, arbitrary expressions can appear inside of a sum type term.
This means reduction is still needed before checking some other terms against
this sum-type-term.
If there's recursion on a sum-type-term, like the Natural number definition:

```ignore
Nat = 'Zero | 'Suc Nat
```

The type-checker will infinitely loop on its reduction.

The above definition will actually be rejected by the termination checker,
but recursion on sum types *have* to be supported because we have been using
it for a long time -- we cannot sacrifice this crucial language feature.

<p style="color: yellowgreen;">
TODO Something need to be written here.
</p>

<br/>
<details>
<summary>About the name, Voile</summary>
This name is inspired from a friend whose username is [*Voile*][vo] (or [*Voileexperiments*][ve]).
However, this is also the name of a library in the [*Scarlet Devil Mansion*][sdm].

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
