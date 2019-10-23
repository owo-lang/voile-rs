use voile_util::level::LiftEx;
use voile_util::loc::{merge_info, Ident};
use voile_util::meta::MetaSolution;
use voile_util::uid::DBI;

use crate::check::monad::TCS;
use crate::syntax::abs::{Abs, LabAbs};
use crate::syntax::core::{CaseSplit, Closure, Neutral, TraverseNeutral, Val, ValInfo, Variants};

/**
Evaluation rules.
$$
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\ty}[0]{\tau}
\newcommand{\eval}[1]{\llbracket #1 \rrbracket} % {\texttt{eval}(#1)}
\newcommand{\piTy}[1]{\Pi \langle #1 \rangle}
\newcommand{\sigTy}[1]{\Sigma \langle #1 \rangle}
\newcommand{\variant}[1]{\textbf{Sum}\\ \\{ #1 \\}}
\newcommand{\record}[1]{\textbf{Rec}\\ \\{ #1 \\}}
\newcommand{\variantR}[1]{\mathbb{Sum}\\ #1}
\newcommand{\recordR}[1]{\mathbb{Rec}\\  #1}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\newcommand{\labels}[1]{\texttt{labels}(#1)}
\newcommand{\fields}[1]{\texttt{fields}(#1)}
\newcommand{\tyLab}[0]{\bar \gamma}
\newcommand{\labVal}[0]{\bar \delta}
\newcommand{\ctyLab}[0]{\gamma}
\newcommand{\clabVal}[0]{\delta}
\newcommand{\caseTr}[0]{\text{ct}}
\newcommand{\caseS}[1]{\langle #1 \rangle}
\newcommand{\case}[4]{\textbf{case}\\ #1\\ #2: #3\\ \textbf{or}\\ #4}
\newcommand{\casevS}[2]{\textbf{split}\\ #1\\ (#2)}
\newcommand{\oneCase}[2]{#1\\ #2}
\newcommand{\oneCaseL}[2]{\oneCase{#1}{\langle #2 \rangle}}
\newcommand{\caseextS}[2]{\textbf{split}\\ (#1)\\ \textbf{or}\\ #2}
\newcommand{\nocases}[0]{\textbf{whatever}}
\newcommand{\recordext}[2]{\record{#1 \mid #2}}
\newcommand{\recExt}[1]{\mid #1}
\newcommand{\variantextS}[2]{\variant{#1, \ldots = #2}}
\newcommand{\recordextS}[2]{\record{#1, \ldots = #2}}
\newcommand{\variantext}[2]{\variant{#1 \mid #2}}
\begin{alignedat}{2}
& \labels{\emptyset} &&= \emptyset \\\\
& \labels{n : a, \tyLab} &&= n : \eval{a}, \labels{\tyLab} \\\\
& \fields{\emptyset} &&= \emptyset \\\\
& \fields{n = a, \labVal} &&= n = \eval{a}, \fields{\labVal} \\\\
& && \\\\
& \eval{a, b} &&= \eval{a}, \eval{b} \\\\
& \eval{a.1} &&= \left\\{\begin{matrix}
  [k.1] & \text{if} & \eval{a} = [k] \\\\
  \alpha_0 & \text{if} & \eval{a} = \alpha\_0, \alpha\_1
\end{matrix}\right\\} \\\\
& \eval{a.2} &&= \left\\{\begin{matrix}
  [k.2] & \text{if} & \eval{a} = [k] \\\\
  \alpha_1 & \text{if} & \eval{a} = \alpha\_0, \alpha\_1
\end{matrix}\right\\} \\\\
& \eval{\lambda \xx. a} &&= \lambda \langle \xx . \eval{a} \rangle \\\\
& \eval{\xx} &&= [\xx] \\\\
& \eval{a\ b} &&= \left\\{\begin{matrix}
  [k\\ \eval{b}] & \text{if} & \eval{a} = [k] \\\\
  \alpha [\xx := \eval{b}] & \text{if} & \eval{a} = \lambda \langle \xx . \alpha \rangle
  \\\\
  \left\\{\begin{matrix}
    \alpha [\xx := \beta] & \text{if} & \eval{b} = n\\ \beta \\\\
    [\casevS{k}{\oneCaseL{n}{\xx. \alpha}, \caseTr}] & \text{if} & \eval{b} = [k] \\\\
  \end{matrix}\right\\} &
  \text{if} & \eval{a} = \lambda \langle \oneCaseL{n}{\xx. \alpha}, \caseTr \rangle
\end{matrix}\right\\} \\\\
& \eval{\piTy{\xx : a.b}} &&= \Pi\\ \eval{a}\\ \langle \xx . \eval{b} \rangle \\\\
& \eval{\sigTy{\xx : a.b}} &&= \Sigma\\ \eval{a}\\ \langle \xx . \eval{b} \rangle \\\\
& \eval{\variantR{ns}} &&= \variantR{ns} \\\\
& \eval{\recordR{ns}} &&= \recordR{ns} \\\\
& \eval{\variant{\tyLab}} &&= \variant{\labels{\tyLab}} \\\\
& \eval{\record{\tyLab}} &&= \record{\labels{\tyLab}} \\\\
& \eval{\variantextS{\tyLab_0}{a}} &&=
\left\\{\begin{matrix}
  [\variantext{\labels{\tyLab_0}}{k}] & \text{if} & \eval{a} = [k] \\\\
  \variant{\labels{\tyLab\_0} \cup \ctyLab\_1} & \text{if} & \eval{a} = \variant{\ctyLab_1} \\\\
  [\variantext{\labels{\tyLab\_0} \cup \ctyLab\_1}{k}] & \text{if} & \eval{a} = [\variantext{\ctyLab_1}{k}] \\\\
\end{matrix}\right\\} \\\\
& \eval{\recordextS{\tyLab_0}{a}} &&=
\left\\{\begin{matrix}
  [\recordext{\labels{\tyLab_0}}{k}] & \text{if} & \eval{a} = [k] \\\\
  \record{\labels{\tyLab\_0} \cup \ctyLab\_1} & \text{if} & \eval{a} = \record{\ctyLab_1} \\\\
  [\recordext{\labels{\tyLab\_0} \cup \ctyLab\_1}{k}] & \text{if} & \eval{a} = [\recordext{\ctyLab_1}{k}] \\\\
\end{matrix}\right\\} \\\\
& \eval{n\\ a} &&= n\\ \eval{a} \\\\
& \eval{\\{ \labVal \\}} &&= \\{ \fields{\labVal} \\} \\\\
& \eval{\\{ \labVal_0\recExt{a} \\}} &&=
\left\\{\begin{matrix}
  \\{ \fields{\labVal\_0} \cup \clabVal\_1 \\} & \text{if} & \eval{a} = \\{ \clabVal_1 \\} \\\\
  [\\{ \fields{\labVal\_0} \cup \clabVal\_1\recExt{k} \\}] & \text{if}
     & \eval{a} = [\\{ n = \alpha, \ctyLab_1\recExt{k} \\}] \\\\
  [\\{ \fields{\labVal_0}\recExt{k} \\}] & \text{if} & \eval{a} = [k] \\\\
\end{matrix}\right\\} \\\\
& \eval{a.n} &&=
\left\\{\begin{matrix}
  [k.n] & \text{if} & \eval{a} = [k] \\\\
  \alpha & \text{if} & \eval{a} = \\{ n = \alpha, \ctyLab \\} \\\\
\end{matrix}\right\\} \\\\
& \eval{\nocases} &&= \lambda \langle \rangle \\\\
& \eval{\case{n}{\xx}{a}{b}} &&=
\left\\{\begin{matrix}
  \lambda \langle \oneCaseL{n}{\xx. \eval{a}}, \caseTr \rangle
  & \text{if} & \eval{b} = \lambda \langle \caseTr \rangle \\\\
  \caseextS{\oneCaseL{n}{\xx. \eval{a}}}{k} & \text{if} & \eval{b} = [k] \\\\
\end{matrix}\right\\} \\\\
& \eval{\ty} &&= \ty
\end{alignedat}
$$

Ensure `abs` is well-typed before invoking this,
otherwise this function may panic or produce ill-typed core term.
*/
fn evaluate(tcs: TCS, abs: Abs) -> (ValInfo, TCS) {
    use Abs::*;
    match abs {
        Type(info, level) => (Val::Type(level).into_info(info), tcs),
        Var(ident, _, i) => {
            let resolved = tcs.local_val(i).ast.clone().attach_dbi(i);
            (resolved.into_info(ident.loc), tcs)
        }
        Rec(info, fields, ext) => {
            let (variants, tcs) = evaluate_variants(tcs, fields);
            let record = Val::Rec(variants);
            match ext {
                None => (record.into_info(info), tcs),
                Some(ext) => {
                    let (ext, tcs) = tcs.evaluate(*ext);
                    (record.rec_extend(ext.ast).into_info(info), tcs)
                }
            }
        }
        Ref(ident, dbi) => (tcs.glob_val(dbi).ast.clone().into_info(ident.loc), tcs),
        Cons(info) => (compile_cons(info), tcs),
        App(info, f, _, a) => {
            // The function should always be compiled to DBI-based terms
            let (f, tcs) = evaluate(tcs, *f);
            let (a, tcs) = evaluate(tcs, *a);
            let (f, tcs) = tcs.expand_global(f.ast);
            let applied = f.apply(a.ast);
            (applied.into_info(info), tcs)
        }
        Dt(info, kind, _, param_plicit, param_ty, ret_ty) => {
            let (param_ty, tcs) = evaluate(tcs, *param_ty);
            let (ret_ty, tcs) = evaluate(tcs, *ret_ty);
            let term = Val::closure_dependent_type(kind, param_plicit, param_ty.ast, ret_ty.ast);
            (term.into_info(info), tcs)
        }
        Pair(info, a, b) => {
            let (a, tcs) = evaluate(tcs, *a);
            let (b, tcs) = evaluate(tcs, *b);
            (Val::pair(a.ast, b.ast).into_info(info), tcs)
        }
        Fst(info, p) => {
            let (p, tcs) = evaluate(tcs, *p);
            let (p, tcs) = tcs.expand_global(p.ast);
            (p.first().into_info(info), tcs)
        }
        Snd(info, p) => {
            let (p, tcs) = evaluate(tcs, *p);
            let (p, tcs) = tcs.expand_global(p.ast);
            (p.second().into_info(info), tcs)
        }
        Proj(info, rec, field) => {
            let (rec, tcs) = evaluate(tcs, *rec);
            let (rec, tcs) = tcs.expand_global(rec.ast);
            (rec.project(field.text).into_info(info), tcs)
        }
        // This branch is not likely to be reached.
        Lam(info, _, _, body) => {
            let (body, tcs) = evaluate(tcs, *body);
            (body.ast.into_info(info), tcs)
        }
        Lift(info, levels, expr) => {
            let (expr, tcs) = evaluate(tcs, *expr);
            let (expr, tcs) = tcs.expand_global(expr.ast);
            (expr.lift(levels).into_info(info), tcs)
        }
        Meta(ident, mi) => {
            if let MetaSolution::Solved(sol) = tcs.meta_context.solution(mi) {
                (sol.clone().into_info(ident.loc), tcs)
            } else {
                (Val::meta(mi).into_info(ident.loc), tcs)
            }
        }
        RowPoly(info, kind, variants, ext) => {
            let (variants, tcs) = evaluate_variants(tcs, variants);
            let row_poly = Val::RowPoly(kind, variants);
            match ext {
                None => (row_poly.into_info(info), tcs),
                Some(ext) => {
                    let (ext, tcs) = tcs.evaluate(*ext);
                    (row_poly.row_extend(ext.ast).into_info(info), tcs)
                }
            }
        }
        RowKind(info, kind, labels) => {
            let labels = labels.into_iter().map(|l| l.text).collect();
            let expr = Val::RowKind(Default::default(), kind, labels);
            (expr.into_info(info), tcs)
        }
        CaseOr(label, _, _, body, or) => {
            let (or, tcs) = tcs.evaluate(*or);
            let (body, tcs) = tcs.evaluate(*body);
            let info = merge_info(&label, &or);
            let mut split = CaseSplit::default();
            split.insert(label.text, Closure::plain(body.ast));
            let lam = Val::case_tree(split);
            (or.ast.split_extend(lam).into_info(info), tcs)
        }
        Whatever(info) => (Val::Lam(Closure::default()).into_info(info), tcs),
    }
}

fn evaluate_variants(mut tcs: TCS, variants: Vec<LabAbs>) -> (Variants, TCS) {
    let mut out_variants = Variants::new();
    for labelled in variants.into_iter() {
        let (expr, new_tcs) = tcs.evaluate(labelled.expr);
        tcs = new_tcs;
        out_variants.insert(labelled.label.text, expr.ast);
    }
    (out_variants, tcs)
}

/// Expand global references to concrete values,
/// like meta references or global references due to recursion.
fn expand_global(tcs: TCS, expr: Val) -> (Val, TCS) {
    use Neutral::*;
    fn go(tcs: &TCS, neut: Neutral) -> Val {
        let java = |neut: Box<Neutral>| go(tcs, *neut);
        match neut {
            Ref(index) => tcs.glob_val(index).ast.clone(),
            Lift(levels, o) => java(o).lift(levels),
            App(o, args) => args.into_iter().fold(java(o), Val::apply),
            Fst(p) => java(p).first(),
            Snd(p) => java(p).second(),
            Proj(r, f) => java(r).project(f),
            Meta(mi) => match &tcs.meta_context.solution(mi) {
                MetaSolution::Solved(val) => *val.clone(),
                // Type-checking error instead of panicking?
                MetaSolution::Unsolved => panic!("Cannot eval unsolved meta: {:?}", mi),
                MetaSolution::Inlined => unreachable!(),
            },
            SplitOn(split, obj) => Val::case_tree(split).apply(java(obj)),
            OrSplit(split, or) => Val::case_tree(split).split_extend(java(or)),
            // Change variants?
            Row(kind, variants, ext) => Val::RowPoly(kind, variants)
                .row_extend_safe(java(ext))
                .unwrap_or_else(|(a, b)| match (a, b) {
                    (Val::RowPoly(_, v), Val::Neut(ext)) => Val::neutral_row_type(kind, v, ext),
                    _ => unreachable!(),
                }),
            // Change fields?
            Rec(fields, ext) => {
                (Val::Rec(fields).rec_extend_safe(java(ext))).unwrap_or_else(|(a, b)| {
                    match (a, b) {
                        (Val::Rec(v), Val::Neut(ext)) => Val::neutral_record(v, ext),
                        _ => unreachable!(),
                    }
                })
            }
            neut => Val::Neut(neut),
        }
    }
    let val = expr.map_neutral(&mut |neut| go(&tcs, neut));
    (val, tcs)
}

/// Evaluate a single constructor as a lambda.
pub fn compile_cons(info: Ident) -> ValInfo {
    let mut text = info.text;
    text.remove(0);
    Val::closure_lam(Val::cons(text, Val::var(DBI(0)))).into_info(info.loc)
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    /// Should be invoked **only** during type-checking,
    /// produce uid-based terms (which can be further type-checked).
    #[inline]
    pub fn evaluate(self, abs: Abs) -> (ValInfo, Self) {
        evaluate(self, abs)
    }

    #[inline]
    pub fn expand_global(self, expr: Val) -> (Val, TCS) {
        expand_global(self, expr)
    }
}
