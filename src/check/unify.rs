use crate::syntax::common::{VarRec, MI};
use crate::syntax::core::{CaseSplit, Closure, Neutral, TraverseNeutral, Val, Variants};

use super::monad::{MetaSolution, TCE, TCM, TCS};

fn check_solution(meta: MI, rhs: Val) -> TCM<()> {
    rhs.try_fold_neutral((), |(), neut| match neut {
        Neutral::Meta(mi) if mi == meta => Err(TCE::MetaRecursion(mi)),
        _ => Ok(()),
    })
}

/// Solve a meta with a specific value.
fn solve_with(mut tcs: TCS, meta: MI, solution: Val) -> TCM {
    // TODO: remove this clone by introducing reference version of `try_fold_neutral`.
    let anticipated_solution = solution.clone().unimplemented_to_glob();
    check_solution(meta, solution)?;
    tcs.solve_meta(meta, anticipated_solution);

    Ok(tcs)
}

/**
Unify two row-polymorphic types.
$$
\newcommand{\Gvdash}[0]{\Gamma \vdash}
\newcommand{\tyck}[4]{#1 \vdash_\texttt{c} #2 : #3 \Rightarrow #4}
\newcommand{\Gtyck}[3]{\tyck{\Gamma}{#1}{#2}{#3}}
\newcommand{\ty}[0]{\tau}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\newcommand{\ctyLab}[0]{\gamma}
\newcommand{\clabVal}[0]{\delta}
\newcommand{\variant}[1]{\textbf{Sum}\\ \\{ #1 \\}}
\newcommand{\record}[1]{\textbf{Rec}\\ \\{ #1 \\}}
\cfrac{
  \Gtyck{A\_0}{\ty}{\cA\_0} \quad
  \Gtyck{A\_1}{\ty}{\cA\_1} \quad
  \Gvdash \cA\_0 \simeq \cA\_1 \quad
  (n : \cA\_0) \notin \ctyLab\_0 \quad
  (n : \cA\_1) \notin \ctyLab\_1
}{
  \cfrac{
    \Gvdash \record{\ctyLab\_0} \simeq \record{\ctyLab\_1}
  }{
    \Gvdash \record{n : \cA\_0, \ctyLab\_0} \simeq \record{n : \cA\_1, \ctyLab\_1}
  } \quad
  \cfrac{
    \Gvdash \variant{\ctyLab\_0} \simeq \variant{\ctyLab\_1}
  }{
    \Gvdash \variant{n : \cA\_0, \ctyLab\_0} \simeq \variant{n : \cA\_1, \ctyLab\_1}
  }
}
$$
*/
fn unify_variants(tcs: TCS, kind: VarRec, subset: &Variants, superset: &Variants) -> TCM {
    subset.iter().try_fold(tcs, |tcs, (name, ty)| {
        let counterpart = superset
            .get(name)
            .ok_or_else(|| TCE::MissingVariant(kind, name.clone()))?;
        tcs.unify(ty, counterpart)
    })
}

/**
Try to unify two well-typed terms,
using a conversion check algorithm.
This may lead to meta variable resolution.
$$
\newcommand{\Gvdash}[0]{\Gamma \vdash}
\newcommand{\cA}[0]{\mathcal A}
\newcommand{\cB}[0]{\mathcal B}
\newcommand{\recordext}[2]{\record{#1 \mid #2}}
\newcommand{\clabVal}[0]{\delta}
\newcommand{\recExt}[1]{\mid #1}
\newcommand{\variantext}[2]{\variant{#1 \mid #2}}
\cfrac{
  \Gvdash \cA\_0 \simeq \cA\_1 \quad \Gvdash \cA\_1 \simeq \cA\_2
}{
  \Gvdash \cA\_0 \simeq \cA\_2
}
\quad
\cfrac{}{\Gvdash \cA \simeq \cA}
\\\\ \space \\\\
\cfrac{}{
  \Gvdash \\{\\} \simeq \\{\\}
} \quad
\cfrac{\Gvdash \alpha \simeq \beta}{
  \Gvdash n\\ \alpha \simeq n\\ \beta
  \quad
  \cfrac{\Gvdash \\{ \clabVal\_0 \\} \simeq \\{ \clabVal\_1 \\}}{
    \Gvdash \\{ n\\ = \alpha, \clabVal\_0 \\}
    \simeq \\{ n\\ = \beta, \clabVal\_1 \\}
  }
}
$$
*/
fn unify(tcs: TCS, a: &Val, b: &Val) -> TCM {
    // use Axiom::Generated as Gen;
    use Neutral::{Axi, Meta, Ref};
    use Val::*;
    match (a, b) {
        (Type(sub_level), Type(super_level)) if sub_level == super_level => Ok(tcs),
        (Neut(Axi(sub)), Neut(Axi(sup))) if sub.unique_id() == sup.unique_id() => Ok(tcs),
        /*
        (Neut(Var(x)), Neut(Var(y))) if x == y => Ok(tcs),
        (Neut(Var(x)), Neut(Axi(Gen(_, y)))) | (Neut(Axi(Gen(_, y))), Neut(Var(x))) if x == y => {
            Ok(tcs)
        }
        */
        (Neut(Ref(x)), Neut(Ref(y))) if x == y => Ok(tcs),
        (Dt(k0, a_plicit, input_a, clos_a), Dt(k1, b_plicit, input_b, clos_b))
            if k0 == k1 && a_plicit == b_plicit =>
        {
            tcs.unify(input_a, input_b)?.unify_closure(clos_a, clos_b)
        }
        (Lam(a), Lam(b)) => unify_closure(tcs, a, b),
        (Cons(_, a), Cons(_, b)) => tcs.unify(&**a, &**b),
        (Pair(a0, a1), Pair(b0, b1)) => tcs.unify(&**a0, &**b0)?.unify(&**a1, &**b1),
        (RowPoly(a_kind, a_variants), RowPoly(b_kind, b_variants))
            if a_kind == b_kind && a_variants.len() == b_variants.len() =>
        {
            tcs.unify_variants(*a_kind, a_variants, b_variants)
        }
        (RowKind(a_level, a_kind, a_labels), RowKind(b_level, b_kind, b_labels))
            if a_level == b_level && a_kind == b_kind && a_labels.len() == b_labels.len() =>
        {
            if a_labels.iter().all(|n| b_labels.contains(n)) {
                Ok(tcs)
            } else {
                Err(TCE::CannotUnify(a.clone(), b.clone()))
            }
        }
        (Rec(a_fields), Rec(b_fields)) if a_fields.len() == b_fields.len() => {
            tcs.unify_variants(VarRec::Record, a_fields, b_fields)
        }
        (term, Neut(Meta(mi))) | (Neut(Meta(mi)), term) => match &tcs.meta_solutions()[mi.0] {
            MetaSolution::Unsolved => solve_with(tcs, *mi, term.clone()),
            MetaSolution::Solved(solution) => {
                let val = *solution.clone();
                tcs.unify(&val, term)
            }
            MetaSolution::Inlined => unreachable!(),
        },
        (Neut(a), Neut(b)) => tcs.unify_neutral(a, b),
        (e, t) => Err(TCE::CannotUnify(e.clone(), t.clone())),
    }
}

/**
Beta-rule in `unify`.
$$
\newcommand{\xx}[0]{\texttt{x}}
\newcommand{\Gvdash}[0]{\Gamma \vdash}
\cfrac{}{
  \Gvdash t \simeq \lambda \xx. t \xx
  \quad
  \Gvdash \lambda \langle \rangle \simeq \lambda \langle \rangle
}
$$
*/
fn unify_closure(tcs: TCS, a: &Closure, b: &Closure) -> TCM {
    use Closure::*;
    match (a, b) {
        (Plain(..), Plain(..)) => {
            let p = Val::fresh_axiom();
            let a = a.instantiate_borrow(&p);
            let b = b.instantiate_cloned(p);
            tcs.unify(&a, &b)
        }
        (Tree(split_a), Tree(split_b)) => tcs.unify_case_split(split_a, split_b),
        (Tree(split), _) | (_, Tree(split)) => {
            let mut tcs = tcs;
            for (label, branch) in split {
                let p = Val::fresh_axiom();
                let cons = Val::cons(label.clone(), p.clone());
                let a = branch.instantiate_cloned(p);
                let b = b.instantiate_cloned(cons);
                tcs = tcs.unify(&a, &b)?;
            }
            Ok(tcs)
        }
    }
}

fn unify_case_split(tcs: TCS, split_a: &CaseSplit, split_b: &CaseSplit) -> TCM {
    if split_a.len() != split_b.len() {
        return Err(TCE::CannotUnify(
            Val::case_tree(split_a.clone()),
            Val::case_tree(split_b.clone()),
        ));
    }
    let mut tcs = tcs;
    for (label, closure_a) in split_a {
        let case_b = split_b
            .get(label)
            .ok_or_else(|| TCE::MissingVariant(VarRec::Variant, label.clone()))?;
        tcs = tcs.unify_closure(closure_a, case_b)?;
    }
    Ok(tcs)
}

/**
Unify two neutral terms.
$$
\newcommand{\casevS}[2]{\textbf{split}\\ #1\\ (#2)}
\newcommand{\caseTr}[0]{\text{ct}}
\newcommand{\clabVal}[0]{\delta}
\newcommand{\caseextS}[2]{\textbf{split}\\ (#1)\\ \textbf{or}\\ #2}
\newcommand{\Gvdash}[0]{\Gamma \vdash}
\newcommand{\recExt}[1]{\mid #1}
\cfrac{
  \Gvdash [k\_0] \simeq [k\_1]
}{
  \Gvdash [k\_0 .n] \simeq [k\_1 .n]
  \quad
  \cfrac{\Gvdash \\{ \clabVal\_0 \\} \simeq \\{ \clabVal_1 \\}}{
    \Gvdash [\\{ \clabVal\_0 \recExt{k\_0} \\}]
    \simeq [\\{ \clabVal\_1 \recExt{k\_1} \\}]
  } \quad
}
\\\\ \space \\\\
\cfrac{
  \Gvdash \lambda c\_0 \simeq \lambda c\_1
  \quad
  \Gvdash \lambda \langle \caseTr_0 \rangle
  \simeq \lambda \langle \caseTr_1 \rangle
}{
  \Gvdash \lambda \langle n\\ c\_0, \caseTr\_0 \rangle
  \simeq \lambda \langle n\\ c\_1, \caseTr\_1 \rangle
}
\\\\ \space \\\\
\cfrac{
  \Gvdash [k\_0] \simeq [k\_1]
  \quad
  \Gvdash \lambda \langle \caseTr_0 \rangle
  \simeq \lambda \langle \caseTr_1 \rangle
}{
  \Gvdash [\caseextS{\caseTr\_0}{k\_0}]
  \simeq [\caseextS{\caseTr\_1}{k\_1}]
  \quad
  \Gvdash [\casevS{k\_0}{\caseTr\_0}]
  \simeq [\casevS{k\_1}{\caseTr\_1}]
}
$$
*/
fn unify_neutral(tcs: TCS, a: &Neutral, b: &Neutral) -> TCM {
    use Neutral::*;
    match (a, b) {
        (Ref(x), Ref(y)) if x == y => Ok(tcs),
        (Lift(x, a), Lift(y, b)) if x == y => tcs.unify_neutral(&**a, &**b),
        (App(f, a), App(g, b)) if a.len() == b.len() => (a.iter().zip(b.iter()))
            .try_fold(tcs.unify_neutral(&*f, &*g)?, |tcs, (x, y)| tcs.unify(x, y)),
        (Rec(a_fields, a_more), Rec(b_fields, b_more)) if a_fields.len() == b_fields.len() => tcs
            .unify_variants(VarRec::Record, a_fields, b_fields)?
            .unify_neutral(&**a_more, &**b_more),
        (Row(a_kind, a_fields, a_more), Row(b_kind, b_fields, b_more))
            if a_kind == b_kind && a_fields.len() == b_fields.len() =>
        {
            tcs.unify_variants(*a_kind, a_fields, b_fields)?
                .unify_neutral(&**a_more, &**b_more)
        }
        (Snd(a), Snd(b)) | (Fst(a), Fst(b)) => tcs.unify_neutral(&**a, &**b),
        (Proj(a, lab_a), Proj(b, lab_b)) if lab_a == lab_b => tcs.unify_neutral(&**a, &**b),
        (SplitOn(split_a, a), SplitOn(split_b, b)) | (OrSplit(split_a, a), OrSplit(split_b, b)) => {
            tcs.unify_case_split(split_a, split_b)?
                .unify_neutral(&**a, &**b)
        }
        (e, t) => Err(TCE::CannotUnify(Val::Neut(e.clone()), Val::Neut(t.clone()))),
    }
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn unify(self, a: &Val, b: &Val) -> TCM {
        unify(self, a, b)
    }

    #[inline]
    fn unify_neutral(self, a: &Neutral, b: &Neutral) -> TCM {
        unify_neutral(self, a, b)
    }

    #[inline]
    fn unify_closure(self, a: &Closure, b: &Closure) -> TCM {
        unify_closure(self, a, b)
    }

    #[inline]
    fn unify_case_split(self, a: &CaseSplit, b: &CaseSplit) -> TCM {
        unify_case_split(self, a, b)
    }

    #[inline]
    pub(crate) fn unify_variants(
        self,
        kind: VarRec,
        subset: &Variants,
        superset: &Variants,
    ) -> TCM {
        unify_variants(self, kind, subset, superset)
    }
}
