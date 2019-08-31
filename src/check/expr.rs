use VarRec::*;

use crate::syntax::abs::{Abs, LabAbs};
use crate::syntax::common::PiSig::*;
use crate::syntax::common::{merge_info, Plicit, SyntaxInfo, ToSyntaxInfo, VarRec};
use crate::syntax::core::{
    CaseSplit, Closure, Fields, Neutral, Val, ValInfo, Variants, TYPE_OMEGA,
};
use crate::syntax::level::{Level, LiftEx};

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
\newcommand{\recordext}[2]{\record{#1 \mid #2}}
\newcommand{\recExt}[1]{\mid #1}
\newcommand{\variant}[1]{\textbf{Sum}\ \{ #1 \}}
\newcommand{\record}[1]{\textbf{Rec}\ \{ #1 \}}
\newcommand{\nocases}[0]{\textbf{whatever}}
\newcommand{\case}[4]{\textbf{case}\\ #1\\ #2: #3\\ \textbf{or}\\ #4}
\newcommand{\variantext}[2]{\variant{#1 \mid #2}}
\newcommand{\ctyLab}[0]{\gamma}
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
\\\\ \space \\\\
\cfrac{
  \Gtyck{B}{\ty}{\cB} \quad
  \Gcheval{b}{\cB}
}{
  \cfrac{
    \Gcheval{a}{\record{\ctyLab}}
  }{
    \Gcheval{
      \\{ n = b\recExt{a} \\}
    }{
      \record{n : \cB, \ctyLab}
    }
  } \quad
  \cfrac{
    \Gcheval{a}{[k]}
  }{
    \Gcheval{
      \\{ n = b\recExt{a} \\}
    }{
      [\recordext{n : \cB}{k}]
    }
  }
}
\\\\ \space \\\\
\cfrac{
  \Gtyck{A}{\ty}{\cA}
}{
  \Gcheval{\nocases}{
    \piTy{\xx : \variant{} . \cA}
  } \quad
  \cfrac{
    \Gtyck{a}{\variant{}}{\alpha}
  }{
    \Gtyck{\nocases\\ a}{\cA}{\alpha}
  }
}
\\\\ \space \\\\
\cfrac{
  \cheval{\Gamma,\xx : \cA\_1}{b}{\cB} \quad
  \Gtyck{A\_1}{\ty}{\cA\_1}
}{
  \cfrac{
    \Gcheval{a}{\piTy{\xx : \variant{\ctyLab} . \cB}}
  }{
    \Gcheval{
      (\case{n}{\xx}{b}{a})
    }{
      \piTy{\xx : \variant{n : \cA\_1, \ctyLab} . \cB}
    }
  } \quad
  \cfrac{
    \Gcheval{a}{\piTy{\xx : [k] . \cB}}
  }{
    \Gcheval{
      (\case{n}{\xx}{b}{a})
    }{
      \piTy{\xx : [\variantext{n : \cA\_1}{k}] . \cB}
    }
  }
}
$$
*/
fn check(mut tcs: TCS, expr: &Abs, expected_type: &Val) -> ValTCM {
    use Abs::*;
    match (expr, expected_type) {
        (Type(info, lower), Val::Type(upper)) => {
            if upper > lower {
                Ok((Val::Type(*lower).into_info_clone(&*info), tcs))
            } else {
                Err(TCE::LevelMismatch(expr.syntax_info(), *lower + 1, *upper))
            }
        }
        (RowKind(info, kind, labels), Val::Type(upper)) if *upper > From::from(0u32) => {
            let labels = labels.iter().map(|l| &l.text).cloned().collect();
            let expr = Val::RowKind(Default::default(), *kind, labels);
            Ok((expr.into_info_clone(&*info), tcs))
        }
        (Meta(ident, mi), _) => Ok((Val::meta(*mi).into_info_clone(&ident.info), tcs)),
        (Pair(info, fst, snd), Val::Dt(Sigma, Plicit::Ex, param_ty, closure)) => {
            let (fst_term, mut tcs) = tcs
                .check(&**fst, &**param_ty)
                .map_err(|e| e.wrap_clone(&*info))?;
            let fst_term_ast = fst_term.ast.clone();
            let snd_ty = closure.instantiate_borrow(&fst_term_ast);
            // This `fst_term.syntax_info()` is probably wrong, but I'm not sure how to fix
            let param_type = param_ty.clone().into_info_clone(&fst_term.syntax_info());
            tcs.local_gamma.push(param_type);
            tcs.local_env.push(fst_term);
            let (snd_term, mut tcs) = tcs
                .check(&**snd, &snd_ty)
                .map_err(|e| e.wrap_clone(&*info))?;
            tcs.pop_local();
            let pair = Val::pair(fst_term_ast, snd_term.ast).into_info_clone(&*info);
            Ok((pair, tcs))
        }
        (Lam(full_info, param_info, uid, body), Val::Dt(Pi, Plicit::Ex, param_ty, ret_ty)) => {
            let param_type = param_ty.clone().into_info_clone(&param_info.info);
            tcs.local_gamma.push(param_type);
            let mocked = Val::postulate(*uid);
            let mocked_term = mocked.clone().into_info_clone(&param_info.info);
            tcs.local_env.push(mocked_term);
            let ret_ty_body = ret_ty.instantiate_cloned(mocked);
            let (lam_term, mut tcs) = tcs
                .check(body, &ret_ty_body)
                .map_err(|e| e.wrap_clone(&*full_info))?;
            tcs.pop_local();
            let lam = Val::closure_lam(lam_term.ast);
            Ok((lam.into_info_clone(&*full_info), tcs))
        }
        (Lam(..), Val::Dt(Pi, Plicit::Im, param_ty, ret_ty)) => {
            let param_type = param_ty
                .clone()
                .into_info(SyntaxInfo::SourceInfo(Default::default()));
            tcs.local_gamma.push(param_type);
            let mocked = Val::fresh_implicit();
            let mocked_term = mocked
                .clone()
                .into_info(SyntaxInfo::SourceInfo(Default::default()));
            tcs.local_env.push(mocked_term);
            let ret_ty_body = ret_ty.instantiate_cloned(mocked);

            let (lam, mut tcs) = tcs.check(&expr.clone(), &ret_ty_body)?;
            tcs.pop_local();
            Ok((lam, tcs))
        }
        (Cons(info), Val::Dt(Pi, ..)) => Ok((compile_cons(info.clone()), tcs)),
        (Dt(info, kind, uid, param_plicit, param, ret), Val::Type(..)) => {
            let (param, mut tcs) = tcs
                .check(&**param, expected_type)
                .map_err(|e| e.wrap_clone(&*info))?;
            tcs.local_gamma.push(param.clone());
            let axiom = Val::postulate(*uid).into_info_clone(&param.syntax_info());
            tcs.local_env.push(axiom);
            let (ret, mut tcs) = tcs
                .check(&**ret, expected_type)
                .map_err(|e| e.wrap_clone(&*info))?;
            tcs.pop_local();
            let dt = Val::closure_dependent_type(*kind, *param_plicit, param.ast, ret.ast)
                .into_info_clone(&*info);
            Ok((dt, tcs))
        }
        (RowPoly(info, Record, variants, ext), Val::RowKind(l, Record, labels)) => {
            check_row_polymorphic_type(tcs, &*info, *l, Record, variants, ext, labels)
        }
        (RowPoly(info, Variant, variants, ext), Val::RowKind(l, Variant, labels)) => {
            check_row_polymorphic_type(tcs, &*info, *l, Variant, variants, ext, labels)
        }
        (RowPoly(info, kind, variants, ext), Val::Type(l)) => {
            check_row_polymorphic_type(tcs, &*info, *l, *kind, variants, ext, &[])
        }
        (Rec(info, fields, more), Val::RowPoly(Record, field_types)) => {
            // Warn about unneeded fields?
            let (nice_fields, rest_field_types, tcs) = check_fields(tcs, fields, field_types)?;
            match more {
                Some(more) => {
                    let more_type = Val::record_type(rest_field_types);
                    let (more, tcs) = tcs.check(&**more, &more_type)?;
                    let record = Val::Rec(nice_fields).rec_extend(more.ast);
                    Ok((record.into_info_clone(&*info), tcs))
                }
                None => check_fields_no_more(&*info, nice_fields, rest_field_types, tcs),
            }
        }
        (Rec(info, fields, more), Val::Neut(Neutral::Row(Record, field_types, more_types))) => {
            let (nice_fields, rest_field_types, tcs) = check_fields(tcs, fields, field_types)?;
            match more {
                Some(more) => {
                    let more_type = if rest_field_types.is_empty() {
                        Val::Neut(*more_types.clone())
                    } else {
                        Val::neutral_record_type(rest_field_types, *more_types.clone())
                    };
                    let (more, tcs) = tcs.check(&**more, &more_type)?;
                    let record = Val::Rec(nice_fields).rec_extend(more.ast);
                    Ok((record.into_info_clone(&*info), tcs))
                }
                None => check_fields_no_more(&*info, nice_fields, rest_field_types, tcs),
            }
        }
        (Lift(info, levels, expr), anything) => {
            let (expr, tcs) = tcs
                .check(&**expr, &anything.clone().lift(0 - *levels))
                .map_err(|e| e.wrap_clone(&*info))?;
            Ok((expr.map_ast(|ast| ast.lift(*levels)), tcs))
        }
        (Whatever(info), Val::Dt(Pi, _, param_ty, ..)) => match &**param_ty {
            Val::RowPoly(Variant, variants) if variants.is_empty() => {
                Ok((Val::Lam(Closure::default()).into_info_clone(&*info), tcs))
            }
            ty => Err(TCE::NotRowType(Variant, (*info).clone(), ty.clone())),
        },
        // How about when `Dt` is `Plicit::Im`?
        (CaseOr(label, binding, uid, body, or), Val::Dt(Pi, Plicit::Ex, param_ty, ret_ty)) => {
            let lam_info = merge_info(binding, &**body);
            let lam = Lam(lam_info, binding.clone(), *uid, body.clone());
            let (variants, ext) = match &**param_ty {
                Val::Neut(Neutral::Row(Variant, variants, ext)) => (variants, Some(&**ext)),
                Val::RowPoly(Variant, variants) => (variants, None),
                ty => {
                    let info = merge_info(label, &**or);
                    return Err(TCE::NotRowType(Variant, info, ty.clone()));
                }
            };
            let mut variants = variants.clone();
            let param_ty = variants
                .remove(&label.text)
                .ok_or_else(|| TCE::MissingVariant(Variant, label.text.clone()))?;
            let input = match ext {
                None => Val::variant_type(variants),
                Some(ext) => Val::neutral_variant_type(variants, ext.clone()),
            };
            let stripped_function = Val::pi(Plicit::Ex, input, ret_ty.clone());
            let dt = Val::pi(Plicit::Ex, param_ty, ret_ty.clone());
            let (body, tcs) = tcs.check(&lam, &dt)?;
            let mut split = CaseSplit::default();
            split.insert(label.text.clone(), Closure::plain(body.ast));
            let ext = Val::Lam(Closure::Tree(split));
            let (or, tcs) = tcs.check(&**or, &stripped_function)?;
            Ok((or.ast.split_extend(ext).into_info(or.info), tcs))
        }
        (expr, anything) => check_fallback(tcs, expr, anything),
    }
}

fn check_fallback(tcs: TCS, expr: &Abs, expected_type: &Val) -> ValTCM {
    let (inferred, tcs) = tcs.infer(expr)?;
    Ok(tcs
        .subtype(&inferred.ast, expected_type)
        .map_err(|e| e.wrap_clone(&inferred.info))?
        .evaluate(expr.clone()))
}

fn check_fields_no_more(
    info: &SyntaxInfo,
    nice_fields: Fields,
    rest_field_types: Variants,
    tcs: TCS,
) -> ValTCM {
    match rest_field_types.keys().next() {
        Some(missing_field) => Err(TCE::MissingVariant(Record, missing_field.clone())),
        None => Ok((Val::Rec(nice_fields).into_info_clone(&info), tcs)),
    }
}

fn check_fields(
    mut tcs: TCS,
    fields: &[LabAbs],
    field_types: &Fields,
) -> TCM<(Fields, Variants, TCS)> {
    let mut nice_fields = Fields::new();
    for field in fields {
        if let Some(ty) = field_types.get(&field.label.text) {
            let key = field.label.text.clone();
            let (field, new_tcs) = tcs.check(&field.expr, ty)?;
            tcs = new_tcs;
            nice_fields.insert(key, field.ast);
        }
    }
    let rest_field_types = field_types
        .iter()
        .filter(|(label, _)| !nice_fields.contains_key(&**label))
        .map(|(label, expr)| (label.clone(), expr.clone()))
        .collect();
    Ok((nice_fields, rest_field_types, tcs))
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
    info: &SyntaxInfo,
    level: Level,
    kind: VarRec,
    variants: &[LabAbs],
    ext: &Option<Box<Abs>>,
    labels: &[String],
) -> ValTCM {
    let mut out_variants = Variants::new();
    for labelled in variants {
        let (val, new_tcs) = tcs
            .check(&labelled.expr, &Val::Type(level))
            .map_err(|e| e.wrap_clone(&info))?;
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
        None => Ok((Val::RowPoly(kind, out_variants).into_info_clone(&info), tcs)),
        Some(ext) => {
            let known_labels = out_variants.keys().chain(labels.iter()).cloned().collect();
            let expected_kind = Val::RowKind(Default::default(), kind, known_labels);
            let (ext, new_tcs) = tcs
                .check(&**ext, &expected_kind)
                .map_err(|e| e.wrap_clone(&info))?;
            let row_poly = Val::RowPoly(kind, out_variants)
                .row_extend(ext.ast)
                .into_info_clone(&info);
            Ok((row_poly, new_tcs))
        }
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
\newcommand{\tyLab}[0]{\bar \gamma}
\newcommand{\ctyLab}[0]{\gamma}
\newcommand{\labVal}[0]{\bar \delta}
\newcommand{\record}[1]{\textbf{Rec}\\ \\{ #1 \\}}
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
\\\\ \space \\\\
\cfrac{
  \Ginfer{a}{\cA} \quad
  \Ginfer{
    \\{\labVal\\}
  }{
    \record{\ctyLab}
  } \quad
  (n : \cA) \notin \ctyLab
}{
  \Ginfer{
    \\{n = a, \labVal\\}
  }{
    \record{n : \cA, \ctyLab}
  }
} \quad
\cfrac{
  \Ginfer{a}{\record{n : \cA, \ctyLab}}
}{
  \Ginfer{a .n}{\cA}
}
$$
*/
fn infer(tcs: TCS, value: &Abs) -> ValTCM {
    use Abs::*;
    let info = value.syntax_info();
    match value {
        Type(_, level) => Ok((Val::Type(*level + 1).into_info_clone(&info), tcs)),
        RowKind(..) => Ok((Val::Type(From::from(1u32)).into_info_clone(&info), tcs)),
        RowPoly(_, kind, variants, more) => {
            let mut labels = Vec::with_capacity(variants.len());
            let mut tcs = tcs;
            let mut max_level = Level::default();
            for variant in variants {
                let (val, new_tcs) = tcs.check(&variant.expr, &TYPE_OMEGA)?;
                tcs = new_tcs;
                labels.push(variant.label.text.clone());
                // Not sure :(
                max_level = max_level.max(val.ast.level());
            }
            let kind_level = max_level + 1;
            match more {
                None => Ok((Val::Type(kind_level).into_info_clone(&info), tcs)),
                Some(more) => {
                    let expected = Val::RowKind(kind_level, *kind, labels);
                    let (_, tcs) = tcs.check(&**more, &expected)?;
                    Ok((Val::Type(kind_level).into_info_clone(&info), tcs))
                }
            }
        }
        Rec(_, fields, ext) => {
            let (ext, tcs) = ext
                .as_ref()
                .map(|abs| tcs.infer(&**abs).map_err(|e| e.wrap_clone(&info)))
                .transpose()?
                .unwrap_or((
                    ValInfo {
                        ast: Default::default(),
                        info: SyntaxInfo::SourceInfo(Default::default()),
                    },
                    Default::default(),
                ));
            let (mut ext_fields, more) = match ext.ast {
                Val::RowPoly(Record, fields) => (fields, None),
                Val::Neut(Neutral::Row(Record, fields, more)) => (fields, Some(*more)),
                e => return Err(TCE::NotRecVal(ext.info, e)),
            };
            let mut tcs = tcs;
            for field in fields {
                if ext_fields.contains_key(&field.label.text) {
                    return Err(TCE::duplicate_field(field.label.clone()));
                }
                let (inferred, new_tcs) =
                    tcs.infer(&field.expr).map_err(|e| e.wrap_clone(&info))?;
                tcs = new_tcs;
                ext_fields.insert(field.label.text.clone(), inferred.ast);
            }
            let ty = match more {
                None => Val::record_type(ext_fields),
                Some(more) => Val::neutral_record_type(ext_fields, more),
            };
            Ok((ty.into_info_clone(&info), tcs))
        }
        Var(_, _, dbi) => {
            let local = tcs.local_type(*dbi).ast.clone().attach_dbi(*dbi);
            Ok((local.into_info_clone(&info), tcs))
        }
        Lam(..) => {
            let mut tcs = tcs;
            let param_meta = tcs.fresh_meta();
            let ret_meta = tcs.fresh_meta();
            // let mocked = Val::postulate(*uid);
            // tcs.local_gamma.push(param_meta.clone().into_info(info));
            // tcs.local_env.push(mocked.clone().into_info(info));
            let pi = Val::pi(Plicit::Ex, param_meta, Closure::plain(ret_meta));
            let (_, tcs) = tcs.check(value, &pi)?;
            // tcs.pop_local();
            Ok((pi.into_info_clone(&info), tcs))
        }
        Lift(_, levels, expr) => {
            let (expr, tcs) = tcs.infer(&**expr).map_err(|e| e.wrap_clone(&info))?;
            Ok((expr.map_ast(|ast| ast.lift(*levels)), tcs))
        }
        Ref(_, dbi) => Ok((tcs.glob_type(*dbi).ast.clone().into_info_clone(&info), tcs)),
        Pair(_, fst, snd) => {
            let (fst_ty, tcs) = tcs.infer(&**fst).map_err(|e| e.wrap_clone(&info))?;
            let (snd_ty, tcs) = tcs.infer(&**snd).map_err(|e| e.wrap_clone(&info))?;
            let sigma = Val::sig(fst_ty.ast, Closure::plain(snd_ty.ast)).into_info_clone(&info);
            Ok((sigma, tcs))
        }
        Fst(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair).map_err(|e| e.wrap_clone(&info))?;
            match pair_ty.ast {
                Val::Dt(Sigma, Plicit::Ex, param_type, ..) => {
                    Ok((param_type.into_info_clone(&info), tcs))
                }
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        Proj(_, record, field) => {
            let (record_ty, tcs) = tcs.infer(&**record).map_err(|e| e.wrap_clone(&info))?;
            match record_ty.ast {
                Val::Neut(Neutral::Row(Record, mut fields, ..))
                | Val::RowPoly(Record, mut fields) => fields
                    .remove(&field.text)
                    .map(|ty| (ty.into_info_clone(&info), tcs))
                    .ok_or_else(|| TCE::MissingVariant(Record, field.text.clone())),
                ast => Err(TCE::NotRowType(Record, record_ty.info, ast)),
            }
        }
        Snd(_, pair) => {
            let (pair_ty, tcs) = tcs.infer(&**pair).map_err(|e| e.wrap_clone(&info))?;
            match pair_ty.ast {
                Val::Dt(Sigma, Plicit::Ex, _, closure) => {
                    // Since we can infer the type of `pair`, it has to be well-typed
                    let (pair_compiled, tcs) = tcs.evaluate(*pair.clone());
                    let fst = pair_compiled.ast.first();
                    Ok((closure.instantiate(fst).into_info_clone(&info), tcs))
                }
                ast => Err(TCE::NotSigma(pair_ty.info, ast)),
            }
        }
        App(_, f, _app_plicit, a) => match &**f {
            Cons(variant_info) => {
                let (a, tcs) = tcs.infer(a).map_err(|e| e.wrap_clone(&info))?;
                let mut variant = Variants::default();
                variant.insert(variant_info.text[1..].to_owned(), a.ast);
                Ok((Val::variant_type(variant).into_info_clone(&info), tcs))
            }
            Whatever(whatever_info) => {
                let empty = Val::Lam(Closure::default());
                let (_, mut tcs) = tcs.check(a, &empty).map_err(|e| e.wrap_clone(&info))?;
                Ok((tcs.fresh_meta().into_info_clone(&*whatever_info), tcs))
            }
            f => {
                let (f_ty, tcs) = tcs.infer(f).map_err(|e| e.wrap_clone(&info))?;
                match f_ty.ast {
                    Val::Dt(Pi, Plicit::Im, _param_type, _closure) => {
                        // todo: need to do sth here otherwise check(f_ty, dt) will fail
                        unimplemented!()
                    }
                    Val::Dt(Pi, Plicit::Ex, param_type, closure) => {
                        let (new_a, tcs) = tcs
                            .check(&**a, &*param_type)
                            .map_err(|e| e.wrap_clone(&info))?;
                        Ok((closure.instantiate(new_a.ast).into_info_clone(&info), tcs))
                    }
                    other => Err(TCE::NotPi(info, other)),
                }
            }
        },
        e => Err(TCE::CannotInfer(info, e.clone())),
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
        (Dt(k0, plicit_a, input_a, clos_a), Dt(k1, plicit_b, input_b, clos_b))
            if k0 == k1 && plicit_a == plicit_b =>
        {
            // Parameter invariance
            let tcs = tcs.unify(input_a, input_b)?;
            let p = Val::fresh_axiom();
            let a = clos_a.instantiate_borrow(&p);
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
