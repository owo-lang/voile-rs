use std::collections::BTreeMap;

use VarRec::*;

use crate::syntax::abs::{Abs, LabAbs};
use crate::syntax::common::PiSig::*;
use crate::syntax::common::{SyntaxInfo, ToSyntaxInfo, VarRec};
use crate::syntax::core::{Closure, LiftEx, TVal, Val, Variants};
use crate::syntax::level::Level;

use super::eval::compile_cons;
use super::monad::{ValTCM, TCE, TCM, TCS};

/**
Check an abstract term against an expected type and produce a well-typed term.
$$
\newcommand{\tyck}[4]{#1 \vdash_\texttt{c} #2 : #3 \Rightarrow #4}
\newcommand{\Gtyck}[3]{\tyck{\Gamma}{#1}{#2}{#3}}
\newcommand{\eval}[1]{\llbracket #1 \rrbracket} % {\texttt{eval}(#1)}
\newcommand{\cheval}[3]{#1 \vdash_\texttt{e} #2 \Rightarrow #3}
\newcommand{\Gcheval}[2]{\cheval{\Gamma}{#1}{#2}}
\newcommand{\subt}[0]{<:}
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\ty}[0]{\tau}
\newcommand{\piTy}[1]{\Pi \langle #1 \rangle}
\newcommand{\sigTy}[1]{\Sigma \langle #1 \rangle}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\cfrac{
  \Gtyck{a}{\cA}{\alpha} \quad
  \Gtyck{b}{\cB[\xx := \alpha]}{\beta}
}{
  \Gcheval{a, b}{\sigTy{\xx : \cA . \cB}}
} \quad
\cfrac{
  \tyck{\Gamma, \xx : \cB}{a}{\cA}{\alpha}
}{
  \Gtyck{
    \lambda \xx. \alpha}{\piTy{\xx : \cB . \cA}
  }{
    \lambda \langle \xx . \alpha \rangle
  }
}
$$
*/
fn check(mut tcs: TCS, expr: &Abs, expected_type: &Val) -> ValTCM {
    use Abs::*;
    match (expr, expected_type) {
        (Type(info, lower), Val::Type(upper)) => {
            if upper > lower {
                Ok((Val::Type(*lower).into_info(*info), tcs))
            } else {
                Err(TCE::LevelMismatch(expr.syntax_info(), *lower + 1, *upper))
            }
        }
        (RowKind(info, kind, labels), Val::Type(upper)) if *upper > From::from(0u32) => {
            let labels = labels.iter().map(|l| &l.text).cloned().collect();
            let expr = Val::RowKind(Default::default(), *kind, labels);
            Ok((expr.into_info(*info), tcs))
        }
        (Meta(ident, mi), _) => Ok((Val::meta(*mi).into_info(ident.info), tcs)),
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
        (Cons(info), Val::Dt(Pi, param_ty, ret_ty)) => {
            check_variant_or_cons(&info.info, param_ty, ret_ty).map_err(|e| e.wrap(info.info))?;
            let cons = compile_cons(info.clone());
            Ok((cons, tcs))
        }
        (Dt(info, kind, uid, param, ret), Val::Type(..)) => {
            let (param, mut tcs) = tcs
                .check(&**param, expected_type)
                .map_err(|e| e.wrap(*info))?;
            tcs.local_gamma.push(param.clone());
            let axiom = Val::postulate(*uid).into_info(param.syntax_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs
                .check(&**ret, expected_type)
                .map_err(|e| e.wrap(*info))?;
            tcs.pop_local();
            let dt = Val::dependent_type(*kind, param.ast, ret.ast).into_info(*info);
            Ok((dt, tcs))
        }
        (RowPoly(info, Record, variants, ext), Val::RowKind(l, Record, labels)) => {
            check_row_polymorphic_type(tcs, *info, *l, Record, variants, ext, labels)
        }
        (RowPoly(info, Variant, variants, ext), Val::RowKind(l, Variant, labels)) => {
            check_row_polymorphic_type(tcs, *info, *l, Variant, variants, ext, labels)
        }
        (RowPoly(info, kind, variants, ext), Val::Type(l)) => {
            check_row_polymorphic_type(tcs, *info, *l, *kind, variants, ext, &[])
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
                .subtype(&inferred.ast, anything)
                .map_err(|e| e.wrap(inferred.info))?;
            Ok(tcs.evaluate(expr.clone()))
        }
    }
}

/**
Extracted from `check`.
$$
\newcommand{\ty}[0]{\tau}
\newcommand{\tyck}[4]{#1 \vdash_\texttt{c} #2 : #3 \Rightarrow #4}
\newcommand{\Gtyck}[3]{\tyck{\Gamma}{#1}{#2}{#3}}
\newcommand{\cheval}[3]{#1 \vdash_\texttt{e} #2 \Rightarrow #3}
\newcommand{\Gcheval}[2]{\cheval{\Gamma}{#1}{#2}}
\newcommand{\variant}[1]{\textbf{Sum}\\ \\{ #1 \\}}
\newcommand{\record}[1]{\textbf{Rec}\\ \\{ #1 \\}}
\newcommand{\variantR}[1]{\mathbb{Sum}\ #1}
\newcommand{\recordR}[1]{\mathbb{Rec}\  #1}
\newcommand{\variantextS}[2]{\variant{#1, \ldots = #2}}
\newcommand{\recordextS}[2]{\record{#1, \ldots = #2}}
\newcommand{\cA}[0]{\mathcal A}
\cfrac{
  n \notin ns \quad \Gtyck{A}{\ty}{\cA}
}{
  \cfrac{
    \Gcheval{B}{\recordR{n,ns}}
  }{
    \Gcheval{\recordextS{n : A}{B}}{\recordR{ns}}
  } \quad
  \cfrac{
    \Gcheval{B}{\variantR{n,ns}}
  }{
    \Gcheval{\variantextS{n : A}{B}}{\variantR{ns}}
  }
}
\\\\
\cfrac{
}{
  \Gtyck{\record{}}{\recordR{ns}}{\record{}} \quad
  \Gtyck{\variant{}}{\variantR{ns}}{\variant{}}
}
$$
*/
fn check_row_polymorphic_type(
    mut tcs: TCS,
    info: SyntaxInfo,
    level: Level,
    kind: VarRec,
    variants: &[LabAbs],
    ext: &Option<Box<Abs>>,
    labels: &[String],
) -> ValTCM {
    let mut out_variants = Variants::new();
    for labelled in variants {
        let (val, new_tcs) = tcs.check(&labelled.expr, &Val::Type(level))?;
        tcs = new_tcs;
        let label = &labelled.label.text;
        if out_variants.contains_key(label) {
            return Err(TCE::OverlappingVariant(val.info, label.clone()));
        } else if labels.contains(label) {
            return Err(TCE::UnexpectedVariant(val.info, label.clone()));
        }
        out_variants.insert(label.clone(), val.ast);
    }
    match ext {
        None => Ok((Val::RowPoly(kind, out_variants).into_info(info), tcs)),
        Some(ext) => {
            let known_labels = out_variants.keys().chain(labels.iter()).cloned().collect();
            let expected_kind = Val::RowKind(Default::default(), kind, known_labels);
            let (ext, new_tcs) = tcs.check(&**ext, &expected_kind)?;
            let row_poly = Val::RowPoly(kind, out_variants)
                .extend(ext.ast)
                .into_info(info);
            Ok((row_poly, new_tcs))
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
Infer type of an abstract term, if it's well-typed.
$$
\newcommand{\tyck}[4]{#1 \vdash_\texttt{c} #2 : #3 \Rightarrow #4}
\newcommand{\Gtyck}[3]{\tyck{\Gamma}{#1}{#2}{#3}}
\newcommand{\eval}[1]{\llbracket #1 \rrbracket} % {\texttt{eval}(#1)}
\newcommand{\infer}[3]{#1 \vdash_\texttt{i} #2 \Leftarrow #3}
\newcommand{\Ginfer}[2]{\infer{\Gamma}{#1}{#2}}
\newcommand{\subt}[0]{<:}
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\ty}[0]{\tau}
\newcommand{\piTy}[1]{\Pi \langle #1 \rangle}
\newcommand{\sigTy}[1]{\Sigma \langle #1 \rangle}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\cfrac{
  \Gtyck{A}{\ty}{\cA}\quad
  \tyck{\Gamma, (\xx : \cA)}{B}{\ty}{\cB}
}{
  \Ginfer{\piTy{\xx : A . B}}{\ty} \quad
  \Ginfer{\sigTy{\xx : A . B}}{\ty}
} \quad
\cfrac{}{\Ginfer{\ty}{\ty}}
\\\\ \space \\\\
\cfrac{
  \Ginfer{a}{\sigTy{\xx : \cA . \cB}}
}{
  \Ginfer{a.1}{\cA} \quad
  \Ginfer{a.2}{\cB[\xx := \eval{a.1}]}
} \quad
\cfrac{
  \Ginfer{a}{\piTy{\xx : \cB . \cA}} \quad
  \Gtyck{b}{\cB}{\beta}
}{
  \Ginfer{a\ b}{\cA [\xx := \eval{b}]}
}
$$
*/
fn infer(mut tcs: TCS, value: &Abs) -> ValTCM {
    use Abs::*;
    let info = value.syntax_info();
    match value {
        Type(_, level) => Ok((Val::Type(*level + 1).into_info(info), tcs)),
        RowKind(..) => Ok((Val::Type(From::from(1u32)).into_info(info), tcs)),
        Var(_, _, dbi) => {
            let local = tcs.local_type(*dbi).ast.clone().attach_dbi(*dbi);
            Ok((local.into_info(info), tcs))
        }
        Lam(..) => {
            let param_meta = tcs.fresh_meta();
            let ret_meta = tcs.fresh_meta();
            // let mocked = Val::postulate(*uid);
            // tcs.local_gamma.push(param_meta.clone().into_info(info));
            // tcs.local_env.push(mocked.clone().into_info(info));
            let pi = Val::pi(param_meta, ret_meta);
            let (_, tcs) = tcs.check(value, &pi)?;
            // tcs.pop_local();
            Ok((pi.into_info(info), tcs))
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
        App(_, f, a) => match &**f {
            Cons(variant_info) => {
                let (a, tcs) = tcs.infer(a).map_err(|e| e.wrap(info))?;
                let mut variant = BTreeMap::default();
                variant.insert(variant_info.text[1..].to_owned(), a.ast);
                Ok((Val::variant_type(variant).into_info(info), tcs))
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

/**
Check if `subtype` is a subtype of `supertype`.
$$
\newcommand{\Gvdash}[0]{\Gamma \vdash}
\newcommand{\tyck}[4]{#1 \vdash_\texttt{c} #2 : #3 \Rightarrow #4}
\newcommand{\Gtyck}[3]{\tyck{\Gamma}{#1}{#2}{#3}}
\newcommand{\subt}[0]{<:}
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\ty}[0]{\tau}
\newcommand{\piTy}[1]{\Pi \langle #1 \rangle}
\newcommand{\sigTy}[1]{\Sigma \langle #1 \rangle}
\newcommand{\variant}[1]{\textbf{Sum}\\ \\{ #1 \\}}
\newcommand{\record}[1]{\textbf{Rec}\\ \\{ #1 \\}}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\newcommand{\ctyLab}[0]{\gamma}
\newcommand{\clabVal}[0]{\delta}
\newcommand{\recordext}[2]{\record{#1 \mid #2}}
\newcommand{\recExt}[1]{\mid #1}
\newcommand{\variantext}[2]{\variant{#1 \mid #2}}
\newcommand{\variantR}[1]{\mathbb{Sum}\ #1}
\newcommand{\recordR}[1]{\mathbb{Rec}\  #1}
\cfrac{
  \Gvdash \cA\_0 \simeq \cA\_1 \quad
  \Gvdash \cB\_0 \subt \cB\_1
}{
  \Gvdash \piTy{
    \xx : \cA\_0 . \cB\_0
  } \subt \piTy{\xx : \cA\_1 . \cB\_1} \quad
  \Gvdash \sigTy{
    \xx : \cA\_0 . \cB\_0
  } \subt \sigTy{\xx : \cA\_1 . \cB\_1}
} \\\\ \space \\\\
\cfrac{
  \Gvdash \cA\_0 \subt \cA\_1 \quad \Gvdash \cA\_1 \subt \cA\_2
}{
  \Gvdash \cA\_0 \subt \cA\_2
} \quad
\cfrac{\Gvdash \cA \simeq \cB}{\Gvdash \cB \simeq \cA \quad \Gvdash \cA \subt \cB}
\\\\ \space \\\\
\cfrac{
  n \notin ns
}{
  \Gvdash \variantR{n,ns} \subt \variantR{ns} \quad
  \Gvdash \recordR{ns} \subt \recordR{n,ns}
}
\\\\ \space \\\\
\cfrac{
  \Gtyck{A}{\ty}{\cA} \quad
  (n : \cA) \notin \ctyLab_0
}{
  \cfrac{
    \Gvdash \variant{\ctyLab\_1} \subt \variant{\ctyLab_0}
  }{
    \Gvdash \variant{\ctyLab\_1} \subt \variant{n : \cA, \ctyLab_0}
  } \quad
  \cfrac{
    \Gvdash \record{\ctyLab\_0} \subt \record{\ctyLab_1}
  }{
    \Gvdash \record{n : \cA, \ctyLab\_0} \subt \record{\ctyLab_1}
  }
}
\\\\
\cfrac{
}{
  \Gvdash \recordR{ns} \subt \ty \quad
  \Gvdash \variantR{ns} \subt \ty
}
$$
*/
fn subtype(tcs: TCS, sub: &Val, sup: &Val) -> TCM {
    use Val::*;
    match (sub, sup) {
        (RowKind(sub_l, ..), Type(sup_l)) | (Type(sub_l), Type(sup_l)) if sub_l <= sup_l => Ok(tcs),
        (RowPoly(Record, sub_vs), RowPoly(Record, sup_vs)) => {
            tcs.unify_variants(Record, sup_vs, sub_vs)
        }
        (RowPoly(Variant, sub_vs), RowPoly(Variant, sup_vs)) => {
            tcs.unify_variants(Variant, sub_vs, sup_vs)
        }
        (Dt(k0, input_a, clos_a), Dt(k1, input_b, clos_b)) if k0 == k1 => {
            // Parameter invariance
            let tcs = tcs.unify(input_a, input_b)?;
            let p = Val::fresh_axiom();
            let a = clos_a.instantiate_cloned_borrow(&p);
            let b = clos_b.instantiate_cloned(p);
            // Return value covariance
            tcs.subtype(&a, &b)
        }
        (e, t) => tcs.unify(e, t),
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
    pub fn subtype(self, sub: &Val, sup: &Val) -> TCM {
        subtype(self, sub, sup)
    }
}
