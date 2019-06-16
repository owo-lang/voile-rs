use crate::syntax::common::{SyntaxInfo, DBI, MI, UID};
use crate::syntax::core::{Axiom, Neutral, Val};

use super::monad::{Gamma, TCE, TCM, TCS};

/// Check that all entries in a spine are bound variables.
fn check_spine(telescope: &Gamma) -> TCM<Vec<UID>> {
    let mut res = Vec::with_capacity(telescope.len());
    for val in telescope {
        match val.ast {
            Val::Neut(Neutral::Axi(Axiom::Generated(uid, ..))) => res.push(uid),
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
    rhs.fold_neutral(Ok(()), |err, neut| {
        err?;
        match neut {
            Neutral::Axi(Axiom::Generated(uid, ..)) if spine.contains(&uid) => {
                Err(TCE::MetaScopeError(meta))
            }
            Neutral::Meta(mi) if mi == meta => Err(TCE::MetaRecursion(mi)),
            _ => Ok(()),
        }
    })
}

/**
Solve a meta with a specific value.
*/
fn solve_with(tcs: TCS, meta: MI, solution: Val) -> TCM {
    let spine = check_spine(&tcs.local_env)?;
    check_solution(meta, spine, solution.clone())?;
    // Here I need to produce a new lambda, using its own local DBI.
    unimplemented!();

    Ok(tcs)
}

/**
Try to unify two terms.
This may lead to meta variable resolution.
*/
fn unify(mut tcs: TCS, a: &Val, b: &Val) -> TCM {
    use crate::syntax::core::Neutral::*;
    use Val::*;
    match (a, b) {
        (Type(sub_level), Type(super_level)) if sub_level == super_level => Ok(tcs),
        (Neut(Axi(sub)), Neut(Axi(sup))) => {
            if sub.unique_id() == sup.unique_id() {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(a.clone(), b.clone()))
            }
        }
        (Neut(Var(sub_dbi)), Neut(Var(super_dbi))) => {
            if sub_dbi == super_dbi {
                Ok(tcs)
            } else {
                Err(TCE::NotSameType(Val::var(*sub_dbi), Val::var(*super_dbi)))
            }
        }
        (Sum(a_variants), Sum(b_variants)) if a_variants.len() == b_variants.len() => {
            for (name, ty) in a_variants {
                let counterpart = b_variants
                    .get(name)
                    .ok_or_else(|| TCE::MissingVariant(name.clone()))?;
                tcs = tcs.unify(ty, counterpart)?;
            }
            Ok(tcs)
        }
        (term, Neut(Meta(mi))) | (Neut(Meta(mi)), term) => solve_with(tcs, *mi, term.clone()),
        // TODO: provide a proper error message
        (e, t) => panic!("Cannot unify `{}` with `{}`.", e, t),
    }
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn unify(self, a: &Val, b: &Val) -> TCM {
        unify(self, a, b)
    }
}
