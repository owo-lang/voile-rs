use crate::syntax::abs::AbsDecl;

pub use self::decl::*;
pub use self::eval::*;
pub use self::expr::*;

/**
$$
\\newcommand{\\xx}[0]{\\texttt{x}}
\\Gamma
\\quad
\\Gamma(\xx)=o
$$
Type-checking monad is a `Result<State, Rrror>`.
*/
pub mod monad;

/// Declaration relevant checking.
///
/// Depends on `expr`.
mod decl;
/**
Tooling functions that depends on neither `decl` nor `expr`.
*/
mod eval;
/**
$$
\\newcommand{\\xx}[0]{\\texttt{x}}
\\newcommand{\\istype}[0]{\\vdash_\\texttt{t}}
\\newcommand{\\Gistype}[0]{\\Gamma \\istype}
\\newcommand{\\tyck}[0]{\\vdash_\\texttt{c}}
\\newcommand{\\Gtyck}[0]{\\Gamma \\tyck}
\\newcommand{\\infer}[0]{\\vdash_\\texttt{i}}
\\newcommand{\\Ginfer}[0]{\\Gamma \\infer}
\\newcommand{\\subtype}[0]{\\vdash_{<:}}
\\newcommand{\\Gsubtype}[0]{\\Gamma \\subtype}
\\newcommand{\\ty}[0]{\\textsf{Type}}
\\newcommand{\\Sum}[0]{\\texttt{Sum}\\ }
\\newcommand{\\merge}[0]{\\texttt{merge}}
\\newcommand{\\inst}[0]{\\texttt{inst}}
\\newcommand{\\ctor}[0]{\\texttt{cons}\\ }
\\newcommand{\\app}[0]{\\texttt{app}}
\\newcommand{\\first}[0]{\\texttt{first}}
\\newcommand{\\second}[0]{\\texttt{second}}
\\Gistype a \\Rightarrow o
\\quad
\\Ginfer a \\Rightarrow o
\\quad
\\Gtyck a:m \\Rightarrow o
$$
Expression/Type relevant checking.
*/
mod expr;

pub fn check_main(decls: Vec<AbsDecl>) -> monad::TCM {
    check_decls(Default::default(), decls)
}
