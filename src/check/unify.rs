use crate::syntax::common::{DBI, MI, UID};
use crate::syntax::core::{lambda_with_n_params, Axiom, Neutral, Val};

use super::monad::{Gamma, MetaSolution, TCE, TCM, TCS};

/// Check that all entries in a spine are bound variables.
fn check_spine(telescope: &Gamma) -> TCM<Vec<UID>> {
    let mut res = Vec::with_capacity(telescope.len());
    for val in telescope {
        match val.ast {
            // `Unimplemented` is probably not what we want here,
            // but I'm not sure if it's gonna affect.
            Val::Neut(Neutral::Axi(axiom)) => res.push(axiom.unique_id()),
            _ => return Err(TCE::MetaWithNonVar(val.info)),
        }
    }
    Ok(res)
}

/**
Scope check + occurs check a solution candidate.

Inputs are a meta, a spine of
variable names (which comes from [check_spine](self::check_spine)) and a RHS
term in normal form. In real implementations it would be a bad idea to solve
metas with normal forms (because of size explosion), but here it's the simplest
thing we can do.

We don't have to worry about shadowing here, because normal
forms have no shadowing by our previous quote implementation.
*/
fn check_solution(meta: MI, spine: Vec<UID>, rhs: Val) -> TCM<()> {
    rhs.try_fold_neutral((), |(), neut| match neut {
        Neutral::Axi(axiom) if !spine.contains(&axiom.unique_id()) => {
            Err(TCE::MetaScopeError(meta))
        }
        Neutral::Meta(mi) if mi == meta => Err(TCE::MetaRecursion(mi)),
        _ => Ok(()),
    })
}

/**
Solve a meta with a specific value.
*/
fn solve_with(mut tcs: TCS, meta: MI, solution: Val) -> TCM {
    use Axiom::*;
    let spine = check_spine(&tcs.local_env)?;
    let anticipated_solution = solution.clone().map_axiom(|axiom| match axiom {
        Generated(uid, _) => {
            let msg = "Bad uid during solution generation.";
            // The right most local var has dbi 0.
            Neutral::Var(DBI(spine.iter().rposition(|i| *i == uid).expect(msg)))
        }
        Postulated(_) => Neutral::Axi(axiom),
        Unimplemented(_, dbi) => Neutral::Ref(dbi),
    });
    check_solution(meta, spine, solution)?;
    // Here I need to produce a new lambda, using its own local DBI.
    tcs.solve_meta(
        meta,
        lambda_with_n_params(tcs.local_len(), anticipated_solution),
    );

    Ok(tcs)
}

/**
Try to unify two well-typed terms.
This may lead to meta variable resolution.
*/
fn unify(mut tcs: TCS, a: &Val, b: &Val) -> TCM {
    use {Neutral::*, Val::*};
    match (a, b) {
        (Type(sub_level), Type(super_level)) if sub_level == super_level => Ok(tcs),
        (Neut(Axi(sub)), Neut(Axi(sup))) if sub.unique_id() == sup.unique_id() => Ok(tcs),
        (Cons(_, a), Cons(_, b)) => tcs.unify(&**a, &**b),
        (Pair(a0, a1), Pair(b0, b1)) => tcs.unify(&**a0, &**b0)?.unify(&**a1, &**b1),
        (Sum(a_variants), Sum(b_variants)) if a_variants.len() == b_variants.len() => {
            for (name, ty) in a_variants {
                let counterpart = b_variants
                    .get(name)
                    .ok_or_else(|| TCE::MissingVariant(name.clone()))?;
                tcs = tcs.unify(ty, counterpart)?;
            }
            Ok(tcs)
        }
        (term, Neut(Meta(mi))) | (Neut(Meta(mi)), term) => {
            match tcs.meta_solutions()[mi.0].clone() {
                MetaSolution::Unsolved => solve_with(tcs, *mi, term.clone()),
                // TODO: Re-check?
                MetaSolution::Solved(_solved) => Ok(tcs),
                MetaSolution::Inlined => unreachable!(),
            }
        }
        (Neut(a), Neut(b)) => tcs.unify_neutral(a, b),
        (e, t) => Err(TCE::CannotUnify(e.clone(), t.clone())),
    }
}

fn unify_neutral(tcs: TCS, a: &Neutral, b: &Neutral) -> TCM {
    use Neutral::*;
    match (a, b) {
        (Ref(x), Ref(y)) if x == y => Ok(tcs),
        (Lift(x, a), Lift(y, b)) if x == y => tcs.unify_neutral(&**a, &**b),
        (App(f, a), App(g, b)) if a.len() == b.len() => {
            let mut tcs = tcs.unify_neutral(&**f, &**g)?;
            for (x, y) in a.iter().zip(b.iter()) {
                tcs = tcs.unify(x, y)?;
            }
            Ok(tcs)
        }
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
