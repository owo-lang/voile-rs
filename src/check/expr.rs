use std::cmp::max;
use std::collections::{BTreeMap, BTreeSet};

use crate::syntax::abs::Abs;
use crate::syntax::common::DtKind::*;
use crate::syntax::common::{SyntaxInfo, ToSyntaxInfo};
use crate::syntax::core::{Closure, LiftEx, TVal, Val};

use super::eval::{compile_cons, compile_variant};
use super::monad::{ValTCM, TCE, TCM, TCS};

/**
$$
\\newcommand{\\cL}{{\\cal L}}
\\newcommand{\\xx}[0]{\\texttt{x}}
\\newcommand{\\istype}[0]{\\vdash_\\texttt{t}}
\\newcommand{\\Gistype}[0]{\\Gamma \\istype}
\\newcommand{\\tyck}[0]{\\vdash_\\texttt{c}}
\\newcommand{\\Gtyck}[0]{\\Gamma \\tyck}
\\newcommand{\\infer}[0]{\\vdash_\\texttt{i}}
\\newcommand{\\Ginfer}[0]{\\Gamma \\infer}
\\newcommand{\\ty}[0]{\\textsf{Type}}
\\newcommand{\\Sum}[0]{\\texttt{Sum}\\ }
\\newcommand{\\merge}[0]{\\texttt{merge}}
\\newcommand{\\inst}[0]{\\texttt{inst}}
\\newcommand{\\cons}[0]{\\texttt{cons}\\ }
\\newcommand{\\app}[0]{\\texttt{app}}
\\cfrac{\\Gtyck a:\\Pi\\ C \\Rightarrow n \\quad
        \\Gtyck b:o \\Rightarrow m}
       {\\Gtyck a\\ b : \\inst(C, m) \\Rightarrow \\app(n, m)}
\\\\ \\space \\\\
\\cfrac{\\Gtyck a:o \\Rightarrow n \\quad
        \\Gamma,n:o \\tyck b:\\inst(C, n) \\Rightarrow m}
       {\\Gtyck a, b : \\Sigma\\ C \\Rightarrow n, m}
\\\\ \\space \\\\
\\cfrac{\\Gamma,[\\xx]:o \\tyck a:\\app(n, [\\xx])}
       {\\Gtyck \\lambda \\xx.a :\\Pi \\lang o \\rightarrow n \\rang
           \\Rightarrow \\lambda \\lang o \\rightarrow n \\rang}
\\\\ \\space \\\\
\\cfrac{\\Gtyck a:o \\Rightarrow n \\quad
        \\Gistype c \\Rightarrow m}
       {\\Gtyck a:o+m \\Rightarrow n}
\\\\ \\space \\\\
\\cfrac{}{\\Gtyck `L:\\Pi \\lang o \\rightarrow n \\rang
          \\Rightarrow \\lambda \\lang o \\rightarrow n \\rang}
\\\\ \\space \\\\
\\cfrac{\\Gistype a \\Rightarrow n \\quad
       \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
      {\\Gtyck (\\Sigma \\xx:a.b):\\ty \\Rightarrow
          \\Sigma \\lang n \\rightarrow m \\rang}
\\\\ \\space \\\\
\\cfrac{\\Gistype a \\Rightarrow n \\quad
       \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
      {\\Gtyck (\\Pi \\xx:a.b):\\ty \\Rightarrow
          \\Pi \\lang n \\rightarrow m \\rang}
$$
Abstract Term -> Core Term under an expected type.

Some additional operations:

$$
\\newcommand{\\merge}[0]{\\texttt{merge}}
\\begin{alignedat}{1}
\\merge((), S) &= S \\\\
\\merge((\`L\\ a, S\_1), S\_2) &= \\merge(S\_1, (\`L a, S\_2))
\\end{alignedat}
$$
*/
fn check(mut tcs: TCS, expr: &Abs, expected_type: &Val) -> ValTCM {
    use Abs::*;
    match (expr, expected_type) {
        (Abs::Type(info, lower), Val::Type(upper)) => {
            if *upper > *lower {
                Ok((Val::Type(*lower).into_info(*info), tcs))
            } else {
                Err(TCE::LevelMismatch(expr.syntax_info(), lower + 1, *upper))
            }
        }
        (Pair(info, fst, snd), Val::Dt(Sigma, param_ty, closure)) => {
            let (fst_term, mut tcs) = tcs.check(&**fst, &**param_ty).map_err(|e| e.wrap(*info))?;
            let fst_term_ast = fst_term.ast.clone();
            let snd_ty = closure.instantiate_cloned(fst_term_ast.clone());
            // This `fst_term.syntax_info()` is probably wrong, but I'm not sure how to fix
            let param_type = param_ty.clone().into_info(fst_term.syntax_info());
            tcs.local_gamma.push(param_type);
            tcs.local_env.push(fst_term);
            let (snd_term, mut tcs) = tcs.check(&**snd, &snd_ty).map_err(|e| e.wrap(*info))?;
            tcs.pop_local();
            let pair = Val::pair(fst_term_ast, snd_term.ast).into_info(*info);
            Ok((pair, tcs))
        }
        (Lam(full_info, param_info, uid, body), Val::Dt(Pi, param_ty, ret_ty)) => {
            let param_type = param_ty.clone().into_info(param_info.info);
            tcs.local_gamma.push(param_type);
            let mocked = Val::postulate(*uid);
            let mocked_term = mocked.clone().into_info(param_info.info);
            tcs.local_env.push(mocked_term);
            let ret_ty_body = ret_ty.instantiate_cloned(mocked);
            let (lam_term, mut tcs) = tcs
                .check(body, &ret_ty_body)
                .map_err(|e| e.wrap(*full_info))?;
            tcs.pop_local();
            let lam = Val::lam(lam_term.ast);
            Ok((lam.into_info(*full_info), tcs))
        }
        (Variant(info), Val::Dt(Pi, param_ty, ret_ty)) => {
            check_variant_or_cons(&info.info, param_ty, ret_ty).map_err(|e| e.wrap(info.info))?;
            let variant = compile_variant(info.clone());
            Ok((variant, tcs))
        }
        (Cons(info), Val::Dt(Pi, param_ty, ret_ty)) => {
            check_variant_or_cons(&info.info, param_ty, ret_ty).map_err(|e| e.wrap(info.info))?;
            let cons = compile_cons(info.clone());
            Ok((cons, tcs))
        }
        (Bot(info), Val::Type(_)) => Ok((Val::bot().into_info(*info), tcs)),
        (Dt(info, kind, uid, param, ret), Val::Type(l)) => {
            let (param, mut tcs) = tcs.check_type(&**param).map_err(|e| e.wrap(*info))?;
            let param_level = param.ast.level();
            if param_level >= *l {
                return Err(TCE::LevelMismatch(*info, param_level - 1, *l));
            }
            tcs.local_gamma.push(param.clone());
            let axiom = Val::postulate(*uid).into_info(param.syntax_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs.check_type(&**ret).map_err(|e| e.wrap(*info))?;
            let ret_level = ret.ast.level();
            if ret_level >= *l {
                return Err(TCE::LevelMismatch(*info, ret_level - 1, *l));
            }
            tcs.pop_local();
            let dt = Val::dependent_type(*kind, param.ast, ret.ast).into_info(*info);
            Ok((dt, tcs))
        }
        (Lift(info, levels, expr), anything) => {
            let (expr, tcs) = tcs
                .check(&**expr, &anything.clone().lift(0 - *levels))
                .map_err(|e| e.wrap(*info))?;
            Ok((expr.map_ast(|ast| ast.lift(*levels)), tcs))
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

fn check_variant_or_cons(info: &SyntaxInfo, param_ty: &TVal, ret_ty: &Closure) -> TCM<()> {
    if !param_ty.is_type() {
        Err(TCE::NotTypeVal(*info, param_ty.clone()))
    } else if !ret_ty.body.is_universe() {
        let ret_ty = ret_ty.instantiate_cloned(Default::default());
        Err(TCE::NotUniverseVal(*info, ret_ty))
    } else {
        Ok(())
    }
}

/**
$$
\\newcommand{\\cL}{{\\cal L}}
\\newcommand{\\xx}[0]{\\texttt{x}}
\\newcommand{\\istype}[0]{\\vdash_\\texttt{t}}
\\newcommand{\\Gistype}[0]{\\Gamma \\istype}
\\newcommand{\\tyck}[0]{\\vdash_\\texttt{c}}
\\newcommand{\\Gtyck}[0]{\\Gamma \\tyck}
\\newcommand{\\infer}[0]{\\vdash_\\texttt{i}}
\\newcommand{\\Ginfer}[0]{\\Gamma \\infer}
\\newcommand{\\ty}[0]{\\textsf{Type}}
\\newcommand{\\Sum}[0]{\\texttt{Sum}\\ }
\\newcommand{\\merge}[0]{\\texttt{merge}}
\\newcommand{\\inst}[0]{\\texttt{inst}}
\\newcommand{\\cons}[0]{\\texttt{cons}\\ }
\\newcommand{\\app}[0]{\\texttt{app}}
\\cfrac{}{\\Gistype \\ty \\Rightarrow \\ty}
\\quad
\\cfrac{}{\\Gistype \\bot \\Rightarrow \\Sum ()}
\\\\ \\space \\\\
\\cfrac{\\Gistype a \\Rightarrow n \\quad
        \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
       {\\Gistype \\Sigma \\xx:a.b \\Rightarrow
           \\Sigma \\lang n \\rightarrow m \\rang}
\\\\ \\space \\\\
\\cfrac{\\Gistype a \\Rightarrow n \\quad
        \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
       {\\Gistype \\Pi \\xx:a.b \\Rightarrow
           \\Pi \\lang n \\rightarrow m \\rang}
\\\\ \\space \\\\
\\cfrac{\\Gistype a \\Rightarrow \\Sum S_1 \\quad
        \\Gistype b \\Rightarrow \\Sum S_2}
       {\\Gistype a+b \\Rightarrow \\Sum \\merge(S\_1, S_2)}
$$
Check if an expression is a valid type expression.
*/
fn check_type(mut tcs: TCS, expr: &Abs) -> ValTCM {
    use Abs::*;
    let info = expr.syntax_info();
    match expr {
        Type(_, level) => Ok((Val::Type(*level).into_info(info), tcs)),
        Var(_, uid, dbi) if tcs.local_is_type(*dbi) => {
            Ok((Val::generate(*uid, *dbi).into_info(info), tcs))
        }
        Lift(_, levels, expr) => {
            let (expr, tcs) = tcs.check_type(&**expr).map_err(|e| e.wrap(info))?;
            Ok((expr.map_ast(|ast| ast.lift(*levels)), tcs))
        }
        Dt(_, kind, uid, param, ret) => {
            let (param, mut tcs) = tcs.check_type(&**param).map_err(|e| e.wrap(info))?;
            tcs.local_gamma.push(param.clone());
            let axiom = Val::postulate(*uid).into_info(param.syntax_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs.check_type(&**ret).map_err(|e| e.wrap(info))?;
            tcs.pop_local();
            let dt = Val::dependent_type(*kind, param.ast, ret.ast).into_info(info);
            Ok((dt, tcs))
        }
        Sum(_, variants) => {
            let mut sum = BTreeMap::default();
            for variant in variants {
                let (variant, new_tcs) = tcs.check_type(variant).map_err(|e| e.wrap(info))?;
                tcs = new_tcs;
                let info = variant.info;
                let mut new = match variant.ast.try_into_sum() {
                    Ok(new) => new,
                    Err(e) => return Err(TCE::NotSumVal(info, e)),
                };
                for new_variant in new.keys() {
                    if sum.contains_key(new_variant) {
                        return Err(TCE::OverlappingVariant(info, new_variant.clone()));
                    }
                }
                sum.append(&mut new);
            }
            Ok((Val::Sum(sum).into_info(info), tcs))
        }
        e => {
            let (ty, tcs) = tcs.infer(e).map_err(|e| e.wrap(info))?;
            if ty.ast.is_universe() {
                Ok(tcs.evaluate(e.clone()))
            } else {
                Err(TCE::NotUniverseVal(info, ty.ast))
            }
        }
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
fn infer(mut tcs: TCS, value: &Abs) -> ValTCM {
    use Abs::*;
    let info = value.syntax_info();
    match value {
        Type(_, level) => Ok((Val::Type(level + 1).into_info(info), tcs)),
        Var(_, _, dbi) => {
            let local = tcs.local_type(*dbi).ast.clone().attach_dbi(*dbi);
            Ok((local.into_info(info), tcs))
        }
        Lift(_, levels, expr) => {
            let (expr, tcs) = tcs.infer(&**expr).map_err(|e| e.wrap(info))?;
            Ok((expr.map_ast(|ast| ast.lift(*levels)), tcs))
        }
        Ref(_, dbi) => Ok((tcs.glob_type(*dbi).ast.clone().into_info(info), tcs)),
        Pair(_, fst, snd) => {
            let (fst_ty, tcs) = tcs.infer(&**fst).map_err(|e| e.wrap(info))?;
            let (snd_ty, tcs) = tcs.infer(&**snd).map_err(|e| e.wrap(info))?;
            let sigma = Val::sig(fst_ty.ast, snd_ty.ast).into_info(info);
            Ok((sigma, tcs))
        }
        Fst(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair).map_err(|e| e.wrap(info))?;
            match pair_ty.ast {
                Val::Dt(Sigma, param_type, ..) => Ok((param_type.into_info(info), tcs)),
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        Snd(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair).map_err(|e| e.wrap(info))?;
            match pair_ty.ast {
                Val::Dt(Sigma, _, closure) => {
                    // Since we can infer the type of `pair`, it has to be well-typed
                    let (pair_compiled, tcs) = tcs.evaluate(*pair.clone());
                    let fst = pair_compiled.ast.first();
                    Ok((closure.instantiate(fst).into_info(info), tcs))
                }
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        Sum(_, variants) => {
            let mut known = BTreeSet::default();
            let mut level = 0;
            for variant in variants {
                let (variant, new_tcs) = tcs.check_type(variant).map_err(|e| e.wrap(info))?;
                tcs = new_tcs;
                level = max(level, variant.ast.level());
                let info = variant.info;
                match variant.ast.try_into_sum() {
                    Ok(variants) => {
                        for (name, _) in variants {
                            if known.contains(&name) {
                                return Err(TCE::OverlappingVariant(info, name));
                            } else {
                                known.insert(name);
                            }
                        }
                    }
                    Err(e) => return Err(TCE::NotSumVal(info, e)),
                }
            }
            Ok((Val::Type(level).into_info(info), tcs))
        }
        App(_, f, a) => match &**f {
            Variant(_) => {
                let (e, tcs) = tcs.check_type(a).map_err(|e| e.wrap(info))?;
                Ok((Val::Type(e.ast.level()).into_info(info), tcs))
            }
            Cons(variant_info) => {
                let (a, tcs) = tcs.infer(a).map_err(|e| e.wrap(info))?;
                let mut variant = BTreeMap::default();
                variant.insert(variant_info.text[1..].to_owned(), a.ast);
                Ok((Val::Sum(variant).into_info(info), tcs))
            }
            f => {
                let (f_ty, tcs) = tcs.infer(f).map_err(|e| e.wrap(info))?;
                match f_ty.ast {
                    Val::Dt(Pi, param_type, closure) => {
                        let (new_a, tcs) =
                            tcs.check(&**a, &*param_type).map_err(|e| e.wrap(info))?;
                        Ok((closure.instantiate(new_a.ast).into_info(info), tcs))
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
        (Neut(Axi(sub)), Neut(Axi(sup))) => {
            if sub.unique_id() == sup.unique_id() {
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
        // Bottom type
        (Sum(variants), sup) => {
            if variants.is_empty() {
                Ok(tcs)
            } else {
                Err(TCE::NotSubtype(Sum(variants.clone()), sup.clone()))
            }
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
