#![doc(
    html_logo_url = "https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/icon.svg?sanitize=true"
)]
/*!
Voile is a dependently-typed programming language evolved from [Mini-TT](https://docs.rs/minitt/).

# Design

### Goal

The focus of Voile is *extensible algebraic data types*.
It can solve the expression problem without using any design patterns (like visitor or
object-algebra in Java, or finally-tagless or dtlac in Haskell).

### Features

Voile is supporting sum-types (coproduct), record types (product) as first-class
language components.

TODO Something need to be written here.

# Implementation

TODO Something need to be written here.

### Problems

TODO Something need to be written here.

#### About the name, Voile
This name is inspired from a friend whose username is [*Voile*][vo] (or [*Voileexperiments*][ve]).
However, this is also the name of a library in the [*Scarlet Devil Mansion*][sdm].

<details>
<summary>The librarian of Voile, the Magic Library</summary>
![](https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/voile-librarian.png)
</details>

 [vo]: https://www.codewars.com/users/Voile
 [ve]: https://github.com/Voileexperiments
 [sdm]: https://en.touhouwiki.net/wiki/Scarlet_Devil_Mansion
*/

/// Abstract syntax, surface syntax, parser and well-typed terms (core language).
pub mod syntax;

/// Type-Checking module.
pub mod check;
