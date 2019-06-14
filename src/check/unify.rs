use crate::syntax::core::Val;

use super::monad::{TCE, TCM, TCS};

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
                Err(TCE::NotSameType(Neut(Var(*sub_dbi)), Neut(Var(*super_dbi))))
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
