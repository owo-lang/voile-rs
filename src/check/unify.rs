use crate::syntax::common::MI;
use crate::syntax::core::{Neutral, Val};

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
Try to unify two well-typed terms.
This may lead to meta variable resolution.
*/
fn unify(tcs: TCS, a: &Val, b: &Val) -> TCM {
    use {Neutral::*, Val::*};
    match (a, b) {
        (Type(sub_level), Type(super_level)) if sub_level == super_level => Ok(tcs),
        (Neut(Axi(sub)), Neut(Axi(sup))) if sub.unique_id() == sup.unique_id() => Ok(tcs),
        (Neut(Var(x)), Neut(Var(y))) if x == y => Ok(tcs),
        (Neut(Ref(x)), Neut(Ref(y))) if x == y => Ok(tcs),
        (Cons(_, a), Cons(_, b)) => tcs.unify(&**a, &**b),
        (Pair(a0, a1), Pair(b0, b1)) => tcs.unify(&**a0, &**b0)?.unify(&**a1, &**b1),
        (Sum(a_variants), Sum(b_variants)) if a_variants.len() == b_variants.len() => {
            a_variants.iter().try_fold(tcs, |tcs, (name, ty)| {
                let counterpart = b_variants
                    .get(name)
                    .ok_or_else(|| TCE::MissingVariant(name.clone()))?;
                tcs.unify(ty, counterpart)
            })
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

fn unify_neutral(tcs: TCS, a: &Neutral, b: &Neutral) -> TCM {
    use Neutral::*;
    match (a, b) {
        (Ref(x), Ref(y)) if x == y => Ok(tcs),
        (Lift(x, a), Lift(y, b)) if x == y => tcs.unify_neutral(&**a, &**b),
        (App(f, a), App(g, b)) if a.len() == b.len() => (a.iter().zip(b.iter()))
            .try_fold(tcs.unify_neutral(&*f, &*g)?, |tcs, (x, y)| tcs.unify(x, y)),
        (Snd(a), Snd(b)) | (Fst(a), Fst(b)) => tcs.unify_neutral(&**a, &**b),
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
}
