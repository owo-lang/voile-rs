pub use self::decl::*;
pub use self::eval::*;
pub use self::expr::*;
pub use self::unify::*;

/**
Type-checking monad is a `Result<State, Rrror>`.
$$
\\newcommand{\\xx}[0]{\\texttt{x}}
\\Gamma
\\quad
\\Gamma(\xx)=o
$$
*/
pub mod monad;

/**
Meta variable solving,
term (definitional) equality comparison.
$$
\Gamma \vdash A \simeq B
$$
*/
mod unify;

/**
Declaration relevant checking.
$$
\\newcommand{\\checkD}[0]{\\vdash_\\texttt{d}}
\\newcommand{\\GcheckD}[0]{\\Gamma \\checkD}
\\GcheckD D\ \textbf{ok}
$$

Depends on `expr`.
*/
mod decl;
/**
Tooling functions that depends on neither `decl` nor `expr`.
$$
\llbracket a \rrbracket = a
$$
*/
mod eval;
/**
Expression/Type relevant checking.
$$
\\newcommand{\\tyck}[0]{\\vdash_\\texttt{c}}
\\newcommand{\\Gtyck}[0]{\\Gamma \\tyck}
\\newcommand{\\infer}[0]{\\vdash_\\texttt{i}}
\\newcommand{\\Ginfer}[0]{\\Gamma \\infer}
\\Ginfer a \\Rightarrow o
\\quad
\\Gtyck a:m \\Rightarrow o
\\quad
\\Gamma \vdash A <: B
$$
*/
mod expr;
