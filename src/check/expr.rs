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
$$
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\istype}[0]{\vdash_\texttt{t}}
\newcommand{\Gistype}[0]{\Gamma \istype}
\newcommand{\tyck}[0]{\vdash_\texttt{c}}
\newcommand{\Gtyck}[0]{\Gamma \tyck}
\newcommand{\infer}[0]{\vdash_\texttt{i}}
\newcommand{\Ginfer}[0]{\Gamma \infer}
\newcommand{\subtype}[0]{\vdash_{<:}}
\newcommand{\Gsubtype}[0]{\Gamma \subtype}
\newcommand{\ty}[0]{\textsf{Type}}
\newcommand{\Sum}[0]{\texttt{Sum}\\ }
\newcommand{\merge}[0]{\texttt{merge}}
\newcommand{\eval}[0]{\texttt{eval}}
\newcommand{\inst}[0]{\texttt{inst}}
\newcommand{\first}[0]{\texttt{first}}
\newcommand{\second}[0]{\texttt{second}}
\newcommand{\ctor}[0]{\texttt{Cons}\\ }
\newcommand{\app}[0]{\texttt{app}}
\\cfrac{\\Gtyck a:o \\Rightarrow n \\quad
        \\Gamma,n:o \\tyck b:\\inst(C, n) \\Rightarrow m}
       {\\Gtyck a, b : \\Sigma\\ C \\Rightarrow n, m}
\\\\ \space \\\\ \space
\\cfrac{\\Gamma,[\\xx]:o \\tyck a:\\app(n, [\\xx])}
       {\\Gtyck \\lambda \\xx.a :\\Pi o. \\lang n \\rang
            \\Rightarrow \\lambda \\lang n \\rang}
\\\\ \space \\\\ \space
\\cfrac{\\Gtyck a:o \\Rightarrow n \\quad
        \\Gistype c \\Rightarrow m}
       {\\Gtyck a:o+m \\Rightarrow n}
\\\\ \space \\\\ \space
\\cfrac{}{\\Gtyck \`\\ctor:\\Pi o. \\lang n \\rang
           \\Rightarrow \\lambda \\lang n \\rang}
\\\\ \space \\\\ \space
\\cfrac{\\Gistype a \\Rightarrow n \\quad
        \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
       {\\Gtyck (\\Sigma \\xx:a.b):\\ty \\Rightarrow
            \\Sigma n. \\lang m \\rang}
\\\\ \space \\\\ \space
\\cfrac{\\Gistype a \\Rightarrow n \\quad
        \\Gamma, [\\xx]:n \\istype b \\Rightarrow m}
       {\\Gtyck (\\Pi \\xx:a.b):\\ty \\Rightarrow
            \\Pi n. \\lang m \\rang}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow n \\quad
        \\Gsubtype n <: m}
       {\\Gtyck a:m \\Rightarrow \\eval(a)}
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
        (Type(info, lower), Val::Type(upper)) => {
            if *upper > *lower {
                Ok((Val::Type(*lower).into_info(*info), tcs))
            } else {
                Err(TCE::LevelMismatch(expr.syntax_info(), *lower + 1, *upper))
            }
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
                .check_subtype(&inferred.ast, anything)
                .map_err(|e| e.wrap(inferred.info))?;
            Ok(tcs.evaluate(expr.clone()))
        }
    }
}

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
Check if an expression is a valid type expression.

TODO: replace with `check(expr, Type(Omega))`.
*/
fn check_type(tcs: TCS, expr: &Abs) -> ValTCM {
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
        RowPoly(_, kind, variants, ext) => {
            check_row_polymorphic_type(tcs, info, Level::Omega, *kind, variants, ext, &[])
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

/**
$$
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\istype}[0]{\vdash_\texttt{t}}
\newcommand{\Gistype}[0]{\Gamma \istype}
\newcommand{\tyck}[0]{\vdash_\texttt{c}}
\newcommand{\Gtyck}[0]{\Gamma \tyck}
\newcommand{\infer}[0]{\vdash_\texttt{i}}
\newcommand{\Ginfer}[0]{\Gamma \infer}
\newcommand{\subtype}[0]{\vdash_{<:}}
\newcommand{\Gsubtype}[0]{\Gamma \subtype}
\newcommand{\ty}[0]{\textsf{Type}}
\newcommand{\Sum}[0]{\texttt{Sum}\\ }
\newcommand{\merge}[0]{\texttt{merge}}
\newcommand{\eval}[0]{\texttt{eval}}
\newcommand{\inst}[0]{\texttt{inst}}
\newcommand{\first}[0]{\texttt{first}}
\newcommand{\second}[0]{\texttt{second}}
\newcommand{\ctor}[0]{\texttt{Cons}\\ }
\newcommand{\app}[0]{\texttt{app}}
\\cfrac{\\Gamma(\\xx) = o}{\\Ginfer \\xx \\Rightarrow o}
\\quad
\\cfrac{}{\\Ginfer \\ty \\Rightarrow \\ty}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow \\Sigma n. \\lang m \\rang}
       {\\Ginfer a\\ .1 \\Rightarrow n}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow n \\quad \\Ginfer b \\Rightarrow m}
       {\\Ginfer a,b \\Rightarrow \\Sigma n. \\lang m \\rang}
\\\\ \space \\\\ \space
\\cfrac{\\Gistype a \\Rightarrow o}{\\Ginfer \`\\ctor a \\Rightarrow \\ty}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow o}{\\Ginfer \\ctor a \\Rightarrow \\sum (\`\\ctor o, ())}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow \\Pi o. \\lang n \\rang \\quad
        \\Gtyck b:o \\Rightarrow m}
       {\\Ginfer a \\ b \\Rightarrow \\inst(n, m)}
\\\\ \space \\\\ \space
\\cfrac{\\Gistype a \\Rightarrow \\Sum S_1 \\quad
        \\Gistype b \\Rightarrow \\Sum S_2}
       {\\Ginfer a+b \\Rightarrow \\ty}
\\\\ \space \\\\ \space
\\cfrac{\\Ginfer a \\Rightarrow \\Sigma n. \\lang m \\rang}
       {\\Ginfer a\\ .2 \\Rightarrow \\inst(m, \\first(a))}
$$
Infer type of a value.
*/
fn infer(mut tcs: TCS, value: &Abs) -> ValTCM {
    use Abs::*;
    let info = value.syntax_info();
    match value {
        Type(_, level) => Ok((Val::Type(*level + 1).into_info(info), tcs)),
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
        RowPoly(_, _, _, _) => unimplemented!(),
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

/// Check if `subtype` is a subtype of `supertype`.
fn check_subtype(tcs: TCS, subtype: &Val, supertype: &Val) -> TCM {
    use Val::*;
    match (subtype, supertype) {
        (RowKind(sub_level, ..), Type(super_level)) | (Type(sub_level), Type(super_level))
            if sub_level <= super_level =>
        {
            Ok(tcs)
        }
        (Dt(k0, input_a, clos_a), Dt(k1, input_b, clos_b)) if k0 == k1 => {
            // Parameter invariance
            let tcs = tcs.unify(input_a, input_b)?;
            let p = Val::fresh_axiom();
            let a = clos_a.instantiate_cloned_borrow(&p);
            let b = clos_b.instantiate_cloned(p);
            // Return value covariance
            tcs.check_subtype(&a, &b)
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
    pub fn check_subtype(self, subtype: &Val, supertype: &Val) -> TCM {
        check_subtype(self, subtype, supertype)
    }

    #[inline]
    pub fn check_type(self, expr: &Abs) -> ValTCM {
        check_type(self, expr)
    }
}
