use std::rc::Rc;

use crate::syntax::abs::AbsDecl;
use crate::syntax::core::{DbiEnv, Term};

use super::expr::{check, check_type};
use super::monad::{GammaItem, TCE, TCM, TCS};

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
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
