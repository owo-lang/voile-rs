use crate::check::monad::{TCE, TCM};
use crate::syntax::common::{ParamKind, DBI};
use crate::syntax::env::NamedEnv_;
use crate::syntax::surf::{Decl, DeclKind, Expr, Param};

use super::ast::*;

pub fn trans(decls: Vec<Decl>) -> TCM<AbstractGlobalEnv> {
    decls
        .iter()
        .try_fold(Default::default(), trans_one_decl)
        .map(|res| res.0)
}

type DeclTCS = (AbstractGlobalEnv, NamedEnv_<DBI>);

fn trans_one_decl((mut result, mut name_map): DeclTCS, decl: &Decl) -> TCM<DeclTCS> {
    let name = decl.name.clone();
    let abs = trans_expr(&decl.body, &result, &name_map)?;

    let dbi = *name_map
        .entry(name.info.text)
        .or_insert_with(|| result.len());
    let decl_info = decl.name.info.clone();
    let original = result.get(&dbi);
    let modified = match (decl.kind, original) {
        (DeclKind::Sign, None) | (DeclKind::Sign, Some(AbsDecl::Sign(_, _))) => {
            AbsDecl::Sign(decl_info, abs)
        }
        (DeclKind::Impl, None) | (DeclKind::Impl, Some(AbsDecl::Impl(_, _))) => {
            AbsDecl::Impl(decl_info, abs)
        }
        (DeclKind::Sign, Some(AbsDecl::Impl(impl_info, impl_abs)))
        | (DeclKind::Sign, Some(AbsDecl::Both(_, _, impl_info, impl_abs))) => {
            AbsDecl::Both(decl_info, abs, impl_info.clone(), impl_abs.clone())
        }
        (DeclKind::Impl, Some(AbsDecl::Sign(sign_info, sign_abs)))
        | (DeclKind::Impl, Some(AbsDecl::Both(sign_info, sign_abs, _, _))) => {
            AbsDecl::Both(sign_info.clone(), sign_abs.clone(), decl_info, abs)
        }
    };
    result.insert(dbi, modified);
    Ok((result, name_map))
}

pub fn trans_expr(expr: &Expr, env: &AbstractGlobalEnv, map: &NamedEnv_<DBI>) -> TCM<Abs> {
    trans_expr_inner(expr, env, map, &Default::default(), &Default::default())
}

fn trans_expr_inner(
    expr: &Expr,
    env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    local_env: &AbstractGlobalEnv,
    local_map: &NamedEnv_<DBI>,
) -> TCM<Abs> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abs::Type(syntax.clone(), *level)),
        Expr::Var(ident) => {
            let name = &ident.info.text;
            if local_map.contains_key(name) {
                Ok(Abs::Local(ident.info.clone(), local_map[name]))
            } else if global_map.contains_key(name) {
                Ok(Abs::Var(ident.info.clone(), global_map[name]))
            } else {
                Err(TCE::LookUpFailed(ident.clone()))
            }
        }
        Expr::App(applied, app_vec) => app_vec.iter().try_fold(
            trans_expr_inner(applied, env, global_map, local_env, local_map)?,
            |result, each_expr| -> TCM<_> {
                let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                Ok(Abs::app(result, abs))
            },
        ),
        // I really hope I can reuse the code with `App` here :(
        Expr::Pipe(startup, pipe_vec) => pipe_vec.iter().try_fold(
            trans_expr_inner(startup, env, global_map, local_env, local_map)?,
            |result, each_expr| -> TCM<_> {
                let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                Ok(Abs::app(result, abs))
            },
        ),
        Expr::Meta(ident) => Ok(Abs::Meta(ident.info.clone())),
        Expr::Cons(ident) => Ok(Abs::Cons(ident.info.clone())),
        Expr::ConsType(ident) => Ok(Abs::ConsType(ident.info.clone())),
        Expr::Bot(ident) => Ok(Abs::Bot(ident.info.clone())),
        Expr::Sum(variants) => {
            let abs_vec = variants
                .iter()
                .map(|expr| trans_expr_inner(expr, env, global_map, local_env, local_map))
                .collect::<TCM<_>>()?;
            Ok(Abs::Sum(abs_vec))
        }
        // TODO: implement these three
        Expr::Tup(_tup_vec) => unimplemented!(),
        Expr::Sig(_, _) => unimplemented!(),
        Expr::Lam(_, _) => unimplemented!(),
        Expr::Pi(params, result) => {
            let mut pi_env = local_env.clone();
            let mut pi_map = local_map.clone();
            let mut pi_vec: Vec<Abs> = params.iter().try_fold(Vec::new(), |pi_vec, param| {
                trans_pi(env, global_map, &mut pi_env, &mut pi_map, pi_vec, param)
            })?;

            // fold from right
            pi_vec.reverse();
            Ok(pi_vec.iter().fold(
                trans_expr_inner(result, env, global_map, &pi_env, &pi_map)?,
                |pi_abs, param| Abs::pi(param.clone(), pi_abs),
            ))
        }
    }
}

fn trans_pi(
    env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    pi_env: &mut AbstractGlobalEnv,
    pi_map: &mut NamedEnv_<DBI>,
    mut pi_vec: Vec<Abs>,
    param: &Param,
) -> TCM<Vec<Abs>> {
    // todo: handle implicit parameter
    // reference implementation:
    // https://github.com/owo-lang/OwO/blob/316e83cf532c447b03a7d210bbc3c4fc1409c861/src/type_check/elaborate.rs#L35-L64
    assert_eq!(param.kind, ParamKind::Explicit);
    let param_ty = trans_expr_inner(&param.ty, env, global_map, &pi_env, &pi_map)?;
    for name in param.names.clone() {
        let param_name = name.info.text.clone();
        let param_dbi: DBI = pi_env.len();
        assert!(!pi_env.contains_key(&param_dbi));
        assert!(!pi_map.contains_key(&param_name));
        pi_env.insert(
            param_dbi,
            AbsDecl::Sign(name.info.clone(), param_ty.clone()),
        );
        pi_map.insert(param_name, param_dbi);
    }
    pi_vec.insert(pi_vec.len(), param_ty);
    Ok(pi_vec)
}
