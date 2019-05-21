use crate::syntax::abs::Abs;
use crate::syntax::common::DtKind::*;
use crate::syntax::common::ToSyntaxInfo;
use crate::syntax::core::{Closure, RedEx, Term};

use super::monad::{TermTCM, TCE, TCM, TCS};
use super::util::unsafe_compile;

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
pub fn check(mut tcs: TCS, expr: Abs, expected_type: Term) -> TermTCM {
    match (expr, expected_type) {
        (Abs::Type(info, lower), Term::Type(upper)) => {
            if upper > lower {
                Ok((Term::Type(lower).into_info(info), tcs))
            } else {
                Err(TCE::LevelMismatch(info, lower + 1, upper))
            }
        }
        (Abs::Pair(info, fst, snd), Term::Dt(Sigma, closure)) => {
            let (fst_term, mut tcs) = check(tcs, *fst, *closure.param_type.clone())?;
            let fst_term_ast = fst_term.ast.clone();
            let snd_ty = closure.body.instantiate(fst_term_ast.clone());
            // This `fst_term.to_info()` is probably wrong, but I'm not sure how to fix
            let param_type = closure.param_type.into_info(fst_term.to_info());
            tcs.local_gamma.push(param_type);
            tcs.local_env.push(fst_term);
            let (snd_term, mut tcs) = check(tcs, *snd, snd_ty)?;
            tcs.pop_local();
            Ok((Term::pair(fst_term_ast, snd_term.ast).into_info(info), tcs))
        }
        (Abs::Lam(_full_info, param_info, name, body), Term::Dt(Pi, ret_ty)) => {
            let param_type = ret_ty.param_type.into_info(param_info.clone());
            tcs.local_gamma.push(param_type);
            let mocked = Term::axiom_with_value(name.uid);
            tcs.local_env.push(mocked.clone().into_info(param_info));
            let (lam_term, mut tcs) = check(tcs, *body, ret_ty.body.instantiate(mocked))?;
            tcs.pop_local();
            Ok((lam_term, tcs))
        }
        (Abs::Local(info, name, dbi), anything) => {
            let inferred = tcs.local_type(dbi).ast.into_info(info);
            let tcs: TCS = check_subtype(tcs, &inferred.ast, &anything)
                .map_err(|e| e.wrap(inferred.info.clone()))?;
            Ok((Term::var(dbi).into_info(inferred.info), tcs))
        }
        (Abs::Var(info, dbi), anything) => {
            let inferred = tcs.glob_type(dbi).ast.into_info(info);
            let tcs: TCS = check_subtype(tcs, &inferred.ast, &anything)
                .map_err(|e| e.wrap(inferred.info.clone()))?;
            Ok((tcs.glob_val(dbi).ast.into_info(inferred.info), tcs))
        }
        (Abs::App(info, f, a), anything) => {
            let app = Abs::App(info, f, a);
            let (inferred, tcs) = infer(tcs, app.clone())?;
            let tcs: TCS =
                check_subtype(tcs, &inferred.ast, &anything).map_err(|e| e.wrap(inferred.info))?;
            Ok(unsafe_compile(tcs, app))
        }
        (Abs::Bot(info), Term::Type(level)) => Ok((Term::Bot(level - 1).into_info(info), tcs)),
        (Abs::Dt(info, kind, name, param, ret), Term::Type(l)) => {
            // TODO: level checking
            let (param, mut tcs) = check_type(tcs, *param)?;
            tcs.local_gamma.push(param.clone());
            let axiom = Term::axiom_with_value(name.uid).into_info(param.to_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = check_type(tcs, *ret)?;
            tcs.pop_local();
            let dt = Term::dependent_type(kind, Closure::new(param.ast, ret.ast)).into_info(info);
            Ok((dt, tcs))
        }
        _ => unimplemented!(),
    }
}

/// Check if an expression is a valid type expression.
pub fn check_type(tcs: TCS, expr: Abs) -> TermTCM {
    match expr {
        Abs::Type(info, level) => Ok((Term::Type(level).into_info(info), tcs)),
        Abs::Bot(info) => Ok((Term::Bot(0).into_info(info), tcs)),
        Abs::Local(_, name, dbi) if tcs.local_is_type(dbi) => {
            // Rust does not allow matching `info` out :(
            let axiom = Term::axiom_with_value(name.uid).into_info(expr.to_info());
            Ok((axiom, tcs))
        }
        Abs::Dt(info, kind, name, param, ret) => {
            let (param, mut tcs) = check_type(tcs, *param)?;
            tcs.local_gamma.push(param.clone());
            let axiom = Term::axiom_with_value(name.uid).into_info(param.to_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = check_type(tcs, *ret)?;
            tcs.pop_local();
            let dt = Term::dependent_type(kind, Closure::new(param.ast, ret.ast)).into_info(info);
            Ok((dt, tcs))
        }
        Abs::ConsType(info) => unimplemented!(),
        e => Err(TCE::NotType(e.to_info(), e)),
    }
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
        Local(info, _, dbi) => Ok((tcs.local_type(dbi).ast.clone().into_info(info), tcs)),
        Var(info, dbi) => Ok((tcs.glob_type(dbi).ast.clone().into_info(info), tcs)),
        Pair(info, fst, snd) => {
            let (fst_ty, tcs) = infer(tcs, *fst)?;
            let (snd_ty, tcs) = infer(tcs, *snd)?;
            let sigma = Term::sig(fst_ty.ast, snd_ty.ast).into_info(info);
            Ok((sigma, tcs))
        }
        Fst(info, pair) => {
            let (pair_ty, tcs) = infer(tcs, *pair)?;
            match pair_ty.ast {
                Term::Dt(Sigma, closure) => Ok((closure.param_type.into_info(info), tcs)),
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        Snd(info, pair) => {
            let (pair_ty, tcs) = infer(tcs, *pair.clone())?;
            match pair_ty.ast {
                Term::Dt(Sigma, closure) => {
                    // Since we can infer the type of `pair`, it has to be well-typed
                    let (pair_compiled, tcs) = unsafe_compile(tcs, *pair);
                    let fst = pair_compiled.ast.first();
                    Ok((closure.body.instantiate(fst).into_info(info), tcs))
                }
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        // TODO: special treatment for `ConsType` and `Cons`.
        App(info, f, a) => match *f {
            f => {
                let (f_ty, tcs) = infer(tcs, f)?;
                match f_ty.ast {
                    Term::Dt(Pi, closure) => {
                        let (new_a, tcs) = check(tcs, *a, *closure.param_type)?;
                        Ok((closure.body.instantiate(new_a.ast).into_info(info), tcs))
                    }
                    other => Err(TCE::NotPi(info, other)),
                }
            }
        },
        e => panic!("Unimplemented inference: `{:?}`.", e),
    }
}

/// Check if `subtype` is a subtype of `supertype`.
pub fn check_subtype(tcs: TCS, subtype: &Term, supertype: &Term) -> TCM {
    use crate::syntax::core::Neutral::*;
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => Ok(tcs),
        (Neut(Axi(uid0)), Neut(Axi(uid1))) => {
            if uid0 == uid1 {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(Neut(Axi(*uid0)), Neut(Axi(*uid1))))
            }
        }
        (Neut(Var(dbi0)), Neut(Var(dbi1))) => {
            if dbi0 == dbi1 {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(Neut(Var(*dbi0)), Neut(Var(*dbi1))))
            }
        }
        _ => unimplemented!(),
    }
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn check(self, expr: Abs, expected_type: Term) -> TermTCM {
        check(self, expr, expected_type)
    }

    #[inline]
    pub fn infer(self, value: Abs) -> TermTCM {
        infer(self, value)
    }

    #[inline]
    pub fn check_subtype(self, subtype: &Term, supertype: &Term) -> TCM {
        check_subtype(self, subtype, supertype)
    }

    #[inline]
    pub fn check_type(self, expr: Abs) -> TermTCM {
        check_type(self, expr)
    }
}
