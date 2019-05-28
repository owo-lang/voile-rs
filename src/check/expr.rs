use crate::syntax::abs::Abs;
use crate::syntax::common::DtKind::*;
use crate::syntax::common::ToSyntaxInfo;
use crate::syntax::core::{Closure, RedEx, Val};

use super::monad::{ValTCM, TCE, TCM, TCS};
use super::util::compile_variant;
use std::collections::BTreeMap;

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
fn check(mut tcs: TCS, expr: &Abs, expected_type: &Val) -> ValTCM {
    match (expr, expected_type) {
        (&Abs::Type(ref info, lower), Val::Type(upper)) => {
            if *upper > lower {
                Ok((Val::Type(lower).into_info(info.clone()), tcs))
            } else {
                Err(TCE::LevelMismatch(expr.to_info(), lower + 1, *upper))
            }
        }
        (Abs::Pair(info, fst, snd), Val::Dt(Sigma, closure)) => {
            let (fst_term, mut tcs) = tcs.check(&**fst, &*closure.param_type)?;
            let fst_term_ast = fst_term.ast.clone();
            let snd_ty = closure.body.clone().instantiate(fst_term_ast.clone());
            // This `fst_term.to_info()` is probably wrong, but I'm not sure how to fix
            let param_type = closure.param_type.clone().into_info(fst_term.to_info());
            tcs.local_gamma.push(param_type);
            tcs.local_env.push(fst_term);
            let (snd_term, mut tcs) = tcs.check(&**snd, &snd_ty)?;
            tcs.pop_local();
            let pair = Val::pair(fst_term_ast, snd_term.ast).into_info(info.clone());
            Ok((pair, tcs))
        }
        (Abs::Lam(full_info, param_info, name, body), Val::Dt(Pi, ret_ty)) => {
            let param_type = ret_ty.param_type.clone().into_info(param_info.clone());
            tcs.local_gamma.push(param_type.clone());
            let mocked = Val::axiom_with_uid(name.uid);
            let mocked_term = mocked.clone().into_info(param_info.clone());
            tcs.local_env.push(mocked_term);
            let ret_ty_body = ret_ty.body.clone().instantiate(mocked);
            let (lam_term, mut tcs) = tcs.check(body, &ret_ty_body)?;
            tcs.pop_local();
            let lam = Val::lam(param_type.ast, lam_term.ast);
            Ok((lam.into_info(full_info.clone()), tcs))
        }
        (Abs::Variant(info), Val::Dt(Pi, ret_ty)) => {
            if !ret_ty.param_type.is_type() {
                Err(TCE::NotTypeVal(info.clone(), *ret_ty.param_type.clone()))
            } else if !ret_ty.body.is_universe() {
                Err(TCE::NotUniverseVal(info.clone(), *ret_ty.body.clone()))
            } else {
                let variant = compile_variant(info.clone(), *ret_ty.param_type.clone());
                Ok((variant, tcs))
            }
        }
        (Abs::Bot(info), Val::Type(level)) => {
            Ok((Val::Bot(level - 1).into_info(info.clone()), tcs))
        }
        (Abs::Dt(info, kind, name, param, ret), Val::Type(l)) => {
            // TODO: level checking
            let (param, mut tcs) = tcs.check_type(&**param)?;
            tcs.local_gamma.push(param.clone());
            let axiom = Val::axiom_with_uid(name.uid).into_info(param.to_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs.check_type(&**ret)?;
            tcs.pop_local();
            let clos = Closure::new(param.ast, ret.ast);
            let dt = Val::Dt(*kind, clos).into_info(info.clone());
            Ok((dt, tcs))
        }
        (expr, anything) => {
            let (inferred, tcs) = tcs.infer(expr)?;
            let tcs: TCS = tcs
                .check_subtype(&inferred.ast, anything)
                .map_err(|e| e.wrap(inferred.info))?;
            Ok(tcs.evaluate(expr.clone()))
        }
    }
}

/// Check if an expression is a valid type expression.
fn check_type(mut tcs: TCS, expr: &Abs) -> ValTCM {
    let info = expr.to_info();
    match expr {
        Abs::Type(_, level) => Ok((Val::Type(*level).into_info(info), tcs)),
        Abs::Bot(_) => Ok((Val::Bot(0).into_info(info), tcs)),
        Abs::Local(_, name, dbi) if tcs.local_is_type(*dbi) => {
            let axiom = Val::axiom_with_index(name.uid, *dbi).into_info(info);
            Ok((axiom, tcs))
        }
        Abs::Dt(_, kind, name, param, ret) => {
            let (param, mut tcs) = tcs.check_type(&**param)?;
            tcs.local_gamma.push(param.clone());
            let axiom = Val::axiom_with_uid(name.uid).into_info(param.to_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs.check_type(&**ret)?;
            tcs.pop_local();
            let dt = Val::dependent_type(*kind, param.ast, ret.ast).into_info(info);
            Ok((dt, tcs))
        }
        Abs::Sum(_, variants) => {
            let mut sum = BTreeMap::default();
            for variant in variants {
                let (variant, new_tcs) = tcs.check_type(variant)?;
                tcs = new_tcs;
                let info = variant.info;
                let e = |e: Val| TCE::NotSumVal(info, e);
                let mut new = variant.ast.try_into_sum().map_err(e)?;
                // TODO: check overlapping variants
                sum.append(&mut new);
            }
            Ok((Val::Sum(sum).into_info(info), tcs))
        }
        e => Err(TCE::NotTypeAbs(info, e.clone())),
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
fn infer(tcs: TCS, value: &Abs) -> ValTCM {
    use crate::syntax::abs::Abs::*;
    let info = value.to_info();
    match value {
        Type(_, level) => Ok((Val::Type(level + 1).into_info(info), tcs)),
        Local(_, _, dbi) => {
            let local = tcs.local_type(*dbi).ast.clone().attach_dbi(*dbi);
            Ok((local.into_info(info), tcs))
        }
        Var(_, dbi) => Ok((tcs.glob_type(*dbi).ast.clone().into_info(info), tcs)),
        Pair(_, fst, snd) => {
            let (fst_ty, tcs) = tcs.infer(&**fst)?;
            let (snd_ty, tcs) = tcs.infer(&**snd)?;
            let sigma = Val::sig(fst_ty.ast, snd_ty.ast).into_info(info);
            Ok((sigma, tcs))
        }
        Fst(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair)?;
            match pair_ty.ast {
                Val::Dt(Sigma, closure) => Ok((closure.param_type.into_info(info), tcs)),
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        Snd(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair)?;
            match pair_ty.ast {
                Val::Dt(Sigma, closure) => {
                    // Since we can infer the type of `pair`, it has to be well-typed
                    let (pair_compiled, tcs) = tcs.evaluate(*pair.clone());
                    let fst = pair_compiled.ast.first();
                    Ok((closure.body.instantiate(fst).into_info(info), tcs))
                }
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        // TODO: special treatment for `Cons`.
        App(_, f, a) => match &**f {
            Variant(_) => {
                let (_, tcs) = tcs.check_type(a)?;
                // TODO: level
                Ok((Val::Type(0).into_info(info), tcs))
            }
            Cons(variant_info) => {
                let (a, tcs) = tcs.infer(a)?;
                let mut variant = BTreeMap::default();
                variant.insert(variant_info.text.clone(), a.ast);
                Ok((Val::Sum(variant).into_info(info), tcs))
            }
            f => {
                let (f_ty, tcs) = tcs.infer(f)?;
                match f_ty.ast {
                    Val::Dt(Pi, closure) => {
                        let (new_a, tcs) = tcs.check(&**a, &closure.param_type)?;
                        Ok((closure.body.instantiate(new_a.ast).into_info(info), tcs))
                    }
                    other => Err(TCE::NotPi(info, other)),
                }
            }
        },
        e => panic!("Unimplemented inference: `{}`.", e),
    }
}

/// Check if `subtype` is a subtype of `supertype`.
fn check_subtype(mut tcs: TCS, subtype: &Val, supertype: &Val) -> TCM {
    use crate::syntax::core::Neutral::*;
    use crate::syntax::core::Val::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => Ok(tcs),
        (Neut(Axi(sub_uid, _)), Neut(Axi(super_uid, _))) => {
            if sub_uid == super_uid {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(subtype.clone(), supertype.clone()))
            }
        }
        (Neut(Var(sub_dbi)), Neut(Var(super_dbi))) => {
            if sub_dbi == super_dbi {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(Neut(Var(*sub_dbi)), Neut(Var(*super_dbi))))
            }
        }
        (Sum(sub_variants), Sum(super_variants)) => {
            for (name, ty) in sub_variants {
                let counterpart = super_variants
                    .get(name)
                    .ok_or_else(|| TCE::MissingVariant(name.clone()))?;
                tcs = tcs.check_subtype(ty, counterpart)?;
            }
            Ok(tcs)
        }
        (e, t) => panic!("Unimplemented subtyping: `{}` against `{}`.", e, t),
    }
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn check(self, expr: &Abs, expected_type: &Val) -> ValTCM {
        check(self, expr, expected_type)
    }

    #[inline]
    pub fn infer(self, value: &Abs) -> ValTCM {
        infer(self, value)
    }

    #[inline]
    pub fn check_subtype(self, subtype: &Val, supertype: &Val) -> TCM {
        check_subtype(self, subtype, supertype)
    }

    #[inline]
    pub fn check_type(self, expr: &Abs) -> ValTCM {
        check_type(self, expr)
    }
}
