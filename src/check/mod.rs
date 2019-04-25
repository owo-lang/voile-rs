use std::rc::Rc;

use crate::syntax::abs::{Abs, AbsDecl};
use crate::syntax::common::{DtKind::*, ParamKind::*};
use crate::syntax::core::{DbiEnv, Term};

use self::monad::{GammaItem, TermTCM, TCE, TCM, TCS};

pub mod monad;

/// Expr -> (well-typed) Term
pub fn check(tcs: TCS, expr: Abs, expected_type: Term) -> TermTCM {
    match (expr, expected_type) {
        // TODO: error message with syntax info
        (Abs::Type(info, lower), Term::Type(upper)) => {
            if upper > lower {
                Ok((tcs, Term::Type(lower).into_info(info)))
            } else {
                Err(TCE::LevelMismatch(info.text, lower + 1, upper))
            }
        }
        (Abs::Pair(info, fst, snd), Term::Dt(Explicit, Sigma, snd_ty)) => {
            let (tcs, fst_term) = check(tcs, *fst, *snd_ty.param_type)?;
            let snd_ty = snd_ty.body.instantiate(fst_term.ast.clone());
            let (tcs, snd_term) = check(tcs, *snd, snd_ty)?;
            Ok((tcs, Term::pair(fst_term.ast, snd_term.ast).into_info(info)))
        }
        (Abs::Bot(info), Term::Type(level)) => Ok((tcs, Term::Bot(level - 1).into_info(info))),
        _ => unimplemented!(),
    }
}

/// Check if an expression is a valid type expression
pub fn check_type(_tcs: TCS, _expr: Abs) -> TermTCM {
    unimplemented!()
}

/// infer type of value
pub fn check_infer(tcs: TCS, value: Abs) -> TermTCM {
    use crate::syntax::abs::Abs::*;
    match value {
        Type(info, level) => Ok((tcs, Term::Type(level + 1).into_info(info))),
        // ConsType(info) => Ok((
        //     tcs,
        //     Term::Lam(Closure::new(Term::gen(0), ClosureBody::new(sum))),
        // )),
        _ => unimplemented!(),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(tcs: TCS, subtype: &Term, supertype: &Term) -> TCM {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => Ok(tcs),
        _ => unimplemented!(),
    }
}

pub fn check_main(decls: Vec<AbsDecl>) -> TCM {
    check_declarations(Default::default(), decls)
}

pub fn check_declarations(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: AbsDecl) -> TCM {
    match decl {
        AbsDecl::Both(sign_info, sign_abs, _, impl_abs) => {
            let new_dbi = tcs.env_size;
            let (mut tcs, val) = check_type(tcs, sign_abs)?;
            tcs.gamma.insert(
                new_dbi,
                GammaItem {
                    dbi: new_dbi,
                    location: sign_info.clone(),
                    r#type: val.ast.clone(),
                },
            );
            tcs = tcs.modify_env(|env| Rc::new(env.cons_rc(Term::mock())));

            let (tcs, val) = check(tcs, impl_abs, val.ast)?;

            tcs.try_modify_env(|env: DbiEnv| match env.substitute_at(new_dbi, val.ast) {
                Ok(env) => Ok(Rc::new(env)),
                Err(_) => Err(TCE::DbiOverflow(env.len(), new_dbi)),
            })
        }
        _ => unreachable!(),
    }
}
