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
Voile is a dependently-typed programming language with a non-dependent version
of row-polymorphism, meta variable resolution and implicit parameter syntax.

# Design

## Goal

The focus of Voile is *extensible algebraic data types* on top of
*dependent types*.
It can solve the expression problem without using any design patterns (like
visitor or object-algebra in Java, or finally-tagless or DTALC in Haskell).

## Features

+ First-class language components
  + Sum/Record
+ Dependent type goodies
  + Pi/Sigma

### Extensible Data Types

Voile supports sum-types (coproduct), record-types (product) and their
instances as first-class language components.

First-class record support is related to [Record Calculus][rec-calc], or
 [Extensible Records][ext-rec], or "first-class labels".
The idea of "extensible record" is that the creation, manipulation and
destruction of "records" can be done locally as an expression, without
the need of declaring a record type globally, like:

$$
\newcommand{\Unit}[0]{\texttt{Rec} \\{ \\}}
\newcommand{\Bool}[0]{\texttt{Sum} \\{ \texttt{true}: \Unit \mid \texttt{false}: \Unit \\}}
\begin{alignedat}{2}
&\texttt{not}&&:\Bool \\\\ \space
& && \rarr \Bool \\\\ \space
&\texttt{ifThenElse}&&:\forall A: \Bool \rarr A \rarr A \rarr A
\end{alignedat}
$$

However, we can still extract the types as global definitions
for readability:

$$
\newcommand{\Unit}[0]{\texttt{Unit}}
\newcommand{\Bool}[0]{\texttt{Bool}}
\begin{alignedat}{2}
&\Unit&&=\texttt{Rec} \\{ \\} \\\\ \space
&\Bool&&=\texttt{Sum} \\{ \texttt{true}: \Unit \mid \texttt{false}: \Unit \\} \\\\ \space
&\texttt{not}&&:\Bool \rarr \Bool \\\\ \space
&\texttt{ifThenElse}&&:\forall A: \Bool \rarr A \rarr A \rarr A
\end{alignedat}
$$

Universal quantification should also support generalizing over a part of the
records (in other words, [row-polymorphism][row-poly]), like:

$$
\newcommand{\xx}[0]{\texttt{X}}
\newcommand{\T}[0]{\texttt{Rec} \\{\xx:A, ...=r\\}}
\begin{alignedat}{2}
&\texttt{getX}&&:\forall A: \T \rarr A \\\\ \space
&\texttt{getX}&&=\lambda s. (s.\xx)
\end{alignedat}
$$

Existing row-polymorphism implementation divides into two groups
according to how they support such generalization,
either by qualifying records/variants with constraints,
or by introducing a standalone "row" (the type of $r$ above) kind
and put all the magic into the new kind.

The study on extensible records has a long history,
but there isn't much research and implementations on the
combination of bidirectional type-checking and row-polymorphism yet.
<br/>
Currently, there are some programming languages, such as [MLPolyR]
(there's a [language spec][spec], a paper [first-class cases][fc-c],
a PhD thesis [type-safe extensible programming][tse] and an
[IntelliJ plugin][ij-dtlc]) or [Rose],
whose case-split are first class
(in the papers it's called "first-class cases").

Row polymorphism is useful because it
solves the expression problem at language level,
which means that library authors
can split the library features into several sub-libraries
-- one core library with many extensions. Library users can
combine the core with extensions they want like a Jigsaw
and exclude everything else (to avoid unwanted dependencies) or create their
own extensions without touching the original codebase.

Here's a simple example Voile program that type-checks:

```text
let Unit = Rec {};
let Bottom = Sum {};

val unit : Unit;
let unit = {| |};

let Bool = Sum { True: Unit; False: Unit; };

val true : Bool;
let true = @True unit;

val false : Bool;
let false = @False unit;

val notTrue : (Sum { False: Unit; } -> Bool)
            -> Bool -> Bool;
let notTrue = \f. case True u: false or f;

val notFalse : (Bottom -> Bool)
             -> Sum { False: Unit; } -> Bool;
let notFalse = \f. case False u: true or f;

val not : Bool -> Bool;
let not = notTrue (notFalse whatever);
```

There's another good [example][ex-2] that uses implicit argument syntax.

Extensible variant type is also useful for simulating exception-handling.

 [ex-2]: https://github.com/owo-lang/voile-rs/blob/master/samples/row-polymorphism/solve-ext-meta.voile
 [rec-calc]: https://dl.acm.org/citation.cfm?id=218572
 [ext-rec]: https://wiki.haskell.org/Extensible_record
 [row-poly]: https://en.wikipedia.org/wiki/Row_polymorphism
 [MLPolyR]: https://github.com/owo-lang/mlpolyr
 [fc-c]: https://people.cs.uchicago.edu/~blume/papers/icfp06.pdf
 [tse]: https://arxiv.org/abs/0910.2654
 [spec]: https://people.cs.uchicago.edu/~blume/classes/spr2005/cmsc22620/docs/langspec.pdf
 [ij-dtlc]: https://github.com/owo-lang/intellij-dtlc
 [Rose]: https://dl.acm.org/citation.cfm?doid=3302515.3290325

# Implementation

Voile's implementation is inspired from [Agda], [mlang] and its
prototype, [minitt].
The language design has been influenced by the [Trex] Haskell extension.

It's ridicules to put an implementation notes here instead of into the rustdoc
of the functions in this crate.

 [Agda]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf
 [MiniAgda]: http://www.cse.chalmers.se/~abela/miniagda
 [mlang]: https://github.com/molikto/mlang
 [minitt]: https://lib.rs/crates/minitt
 [Trex]: https://www.microsoft.com/en-us/research/publication/lightweight-extensible-records-for-haskell

<br/>
<span>
<details>
<summary>About the name, Voile</summary>
<span>
This language is named after a person whose username is <a
href="https://www.codewars.com/users/Voile"><em>Voile</em></a> (or
<a href="https://github.com/Voileexperiments"><em>Voileexperiments</em></a>).
However, this is also the name of a library of the
<a href="https://en.touhouwiki.net/wiki/Scarlet_Devil_Mansion"><em>
Scarlet Devil Mansion</em></a>.
</span>
<span><details>
<summary>The librarian of Voile, the Magic Library</summary>
<img src=
"https://raw.githubusercontent.com/owo-lang/voile-rs/master/rustdoc/voile-librarian.png"
alt=""></img>
</details></span>
</details></span>
*/

#[macro_use]
extern crate voile_util;

/// Abstract syntax, surface syntax,
/// parser and well-typed terms (core language).
pub mod syntax;

/// Type-Checking module.
pub mod check;
