use crate::syntax::abs::Abs;
use crate::syntax::common::{DtKind::*, ParamKind::*};
use crate::syntax::core::{RedEx, Term};

use super::monad::{TermTCM, TCE, TCM, TCS};

/// $$
/// \newcommand\U{\textsf{Type}}
/// \cfrac{i < j}{\Gamma \vdash \U\_i : \U\_j \rightsquigarrow \U\_i}
/// \quad
/// \cfrac{}{\Gamma \vdash\bot:\U\_i \rightsquigarrow \bot\_i}
/// \newline
/// \cfrac{\Gamma \vdash a:A \quad \Gamma,a:A \vdash b:B(a)}
///      {\Gamma \vdash (a,b):\Sigma (a:A).B(a) \rightsquigarrow (a,b)}
/// $$
/// Abstract Term -> Core Term under an expected type.
pub fn check(tcs: TCS, expr: Abs, expected_type: Term) -> TermTCM {
    match (expr, expected_type) {
        (Abs::Type(info, lower), Term::Type(upper)) => {
            if upper > lower {
                Ok((Term::Type(lower).into_info(info), tcs))
            } else {
                Err(TCE::LevelMismatch(info, lower + 1, upper))
            }
        }
        (Abs::Pair(info, fst, snd), Term::Dt(Explicit, Sigma, snd_ty)) => {
            let (fst_term, tcs) = check(tcs, *fst, *snd_ty.param_type)?;
            let snd_ty = snd_ty.body.reduce(fst_term.ast.clone());
            let (snd_term, tcs) = check(tcs, *snd, snd_ty)?;
            Ok((Term::pair(fst_term.ast, snd_term.ast).into_info(info), tcs))
        }
        (Abs::Var(info, dbi), anything) => {
            let (inferred, tcs) = infer(tcs, Abs::Var(info, dbi))?;
            let tcs = check_subtype(tcs, &inferred.ast, &anything)?;
            Ok((Term::var(dbi).into_info(inferred.info), tcs))
        }
        (Abs::Bot(info), Term::Type(level)) => Ok((Term::Bot(level - 1).into_info(info), tcs)),
        _ => unimplemented!(),
    }
}

/// Check if an expression is a valid type expression.
pub fn check_type(_tcs: TCS, _expr: Abs) -> TermTCM {
    unimplemented!()
}

/// $$
/// \newcommand\U{\textsf{Type}}
/// \newcommand\G[2]{\Gamma \vdash #1 : #2}
/// \cfrac{}{\G{\U\_i}{\U\_{i+1}}}
/// \quad
/// \cfrac{}{\G{[n]}{\textsf{type}(\Gamma, n)}}
/// \newline
/// \cfrac{\G{a}{A} \quad \G{b}{B}}{\G{(a,b)}{\Sigma A.B}}
/// \quad
/// \cfrac{\G{a}{\Sigma A.B}}{\G{a\textsf{\.1}}{A}}
/// $$
/// Infer type of a value.
pub fn infer(tcs: TCS, value: Abs) -> TermTCM {
    use crate::syntax::abs::Abs::*;
    match value {
        Type(info, level) => Ok((Term::Type(level + 1).into_info(info), tcs)),
        Local(info, dbi) => Ok((tcs.local_gamma[dbi].r#type.clone().into_info(info), tcs)),
        Var(info, dbi) => Ok((tcs.gamma[dbi].r#type.clone().into_info(info), tcs)),
        Pair(info, fst, snd) => {
            let (fst_ty, tcs) = infer(tcs, *fst)?;
            let (snd_ty, tcs) = infer(tcs, *snd)?;
            let sigma = Term::sig(Explicit, fst_ty.ast, snd_ty.ast).into_info(info);
            Ok((sigma, tcs))
        }
        Fst(info, pair) => {
            let (pair_ty, tcs) = infer(tcs, *pair)?;
            match pair_ty.ast {
                Term::Dt(Explicit, Sigma, closure) => Ok((closure.param_type.into_info(info), tcs)),
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        // TODO: special treatment for `ConsType` and `Cons`.
        App(info, f, a) => match *f {
            f => {
                let (f_ty, tcs) = infer(tcs, f)?;
                match f_ty.ast {
                    Term::Dt(Explicit, Pi, closure) => {
                        let (new_a, tcs) = check(tcs, *a, *closure.param_type)?;
                        Ok((closure.body.reduce(new_a.ast).into_info(info), tcs))
                    }
                    other => Err(TCE::NotPi(info, other)),
                }
            }
        },
        _ => unimplemented!(),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(tcs: TCS, subtype: &Term, supertype: &Term) -> TCM {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => Ok(tcs),
        _ => unimplemented!(),
    }
}
