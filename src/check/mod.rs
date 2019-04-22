use self::monad::{GammaItem, TermTCM, TCE, TCM, TCS};
use crate::syntax::abs::Abs;
use crate::syntax::common::{DtKind, ParamKind};
use crate::syntax::core::{DbiEnv, Term};
use crate::syntax::surf::Decl;

pub mod monad;

/// Expr -> (well-typed) Term
pub fn check(tcs: TCS, expr: Abs, expected_type: Term) -> TermTCM {
    match (expr, expected_type) {
        // TODO: error message with syntax info
        (Abs::Type(_, lower), Term::Type(upper)) if upper > lower => Ok((tcs, Term::Type(lower))),
        (Abs::Pair(_, fst, snd), Term::Dt(ParamKind::Explicit, DtKind::Sigma, fst_ty, snd_ty)) => {
            let (tcs, fst_term) = check(tcs, *fst, *fst_ty)?;
            let snd_ty = snd_ty.body.instantiate(fst_term.clone());
            let (tcs, snd_term) = check(tcs, *snd, snd_ty)?;
            Ok((tcs, Term::pair(fst_term, snd_term)))
        }
        _ => unimplemented!(),
    }
}

/// Check if an expression is a valid type expression
pub fn check_type(_tcs: TCS, _expr: Abs) -> TermTCM {
    unimplemented!()
}

/// infer type of value
pub fn check_infer(tcs: TCS, value: Term) -> TermTCM {
    use crate::syntax::core::Term::*;
    match value {
        Type(level) => Ok((tcs, Type(level + 1))),
        _ => Err(TCE::CouldNotInfer(value)),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(tcs: TCS, subtype: Term, supertype: Term) -> TermTCM {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => {
            Ok((tcs, Type(super_level)))
        }
        _ => unimplemented!(),
    }
}

pub fn check_main(decls: Vec<Decl>) -> TCM {
    check_declarations(Default::default(), decls)
}

pub fn check_declarations(mut tcs: TCS, decls: Vec<Decl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: Decl) -> TCM {
    use crate::syntax::surf::DeclKind::*;
    let name = decl.name.clone();
    match decl.kind {
        Sign => {
            let (mut tcs, val) = check_type(tcs, decl.body)?;
            tcs.gamma.insert(
                name.info.text.clone(),
                GammaItem {
                    dbi: tcs.env_size,
                    location: name.info,
                    r#type: val,
                },
            );
            tcs = tcs.modify_env(|env| env.cons(Term::mock()));
            Ok(tcs)
        }
        Impl => {
            let ctx_ty = tcs.gamma.get(&name.info.text).cloned();
            ctx_ty.map_or(Err(TCE::TypeNotInGamma(name)), |ctx_ty| {
                let dbi = ctx_ty.dbi;
                let (tcs, val) = check(tcs, decl.body, ctx_ty.r#type)?;
                // This is "replacing the type signature's corresponded value with a well-typed term"
                tcs.try_modify_env(|env: DbiEnv| match env.substitute_at(dbi, val) {
                    Ok(env) => Ok(env),
                    Err(_) => Err(TCE::DbiOverflow(env.len(), dbi)),
                })
            })
        }
    }
}
