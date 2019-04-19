use crate::check::monad::{TCE, TCM};
use crate::syntax::abs::ast::*;
use crate::syntax::common::{ParamKind, DBI};
use crate::syntax::env::NamedEnv_;
use crate::syntax::surf::{Decl, DeclKind, Expr, Param};

/// todo: replace to proper location
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
    let abs_decl = result.get(&dbi);
    result.insert(
        dbi,
        match (decl.kind, abs_decl) {
            (DeclKind::Sign, None) | (DeclKind::Sign, Some(AbstractDecl::Sign(_))) => {
                AbstractDecl::Sign(abs)
            }
            (DeclKind::Impl, None) | (DeclKind::Impl, Some(AbstractDecl::Impl(_))) => {
                AbstractDecl::Impl(abs)
            }
            (DeclKind::Sign, Some(AbstractDecl::Impl(impl_abs)))
            | (DeclKind::Sign, Some(AbstractDecl::Both(_, impl_abs))) => {
                AbstractDecl::Both(abs, impl_abs.clone())
            }
            (DeclKind::Impl, Some(AbstractDecl::Sign(sign_abs)))
            | (DeclKind::Impl, Some(AbstractDecl::Both(sign_abs, _))) => {
                AbstractDecl::Both(sign_abs.clone(), abs)
            }
        },
    );
    Ok((result, name_map))
}

pub fn trans_expr(expr: &Expr, env: &AbstractGlobalEnv, map: &NamedEnv_<DBI>) -> TCM<Abstract> {
    trans_expr_inner(expr, env, map, &Default::default(), &Default::default())
}

fn trans_expr_inner(
    expr: &Expr,
    env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    local_env: &AbstractGlobalEnv,
    local_map: &NamedEnv_<DBI>,
) -> TCM<Abstract> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abstract::Type(syntax.clone(), *level)),
        Expr::Var(ident) => {
            let name = &ident.info.text;
            if local_map.contains_key(name) {
                Ok(Abstract::Local(ident.info.clone(), local_map[name]))
            } else if global_map.contains_key(name) {
                Ok(Abstract::Var(ident.info.clone(), global_map[name]))
            } else {
                Err(TCE::LookUpFailed(ident.clone()))
            }
        }
        Expr::App(app_vec) => app_vec
            .iter()
            .try_fold(None::<Abstract>, |result, each_expr| -> TCM<_> {
                let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                Ok(match result {
                    // First item in vec
                    None => Some(abs),
                    // Second or other, reduce to Right
                    Some(left_abs) => Some(Abstract::app(left_abs, abs)),
                })
            })
            // Because it's guaranteed to be non-empty.
            .map(std::option::Option::unwrap),
        Expr::Pipe(pipe_vec) => {
            let mut app_vec = pipe_vec.clone();
            app_vec.reverse();
            trans_expr_inner(&Expr::App(app_vec), env, global_map, local_env, local_map)
        }
        Expr::Meta(ident) => Ok(Abstract::Meta(ident.info.clone())),
        Expr::Cons(ident) => Ok(Abstract::Cons(ident.info.clone())),
        Expr::ConsType(ident) => Ok(Abstract::ConsType(ident.info.clone())),
        Expr::Bot(ident) => Ok(Abstract::Bot(ident.info.clone())),
        Expr::Sum(expr_vec) => {
            let abs_vec = expr_vec.iter().try_fold(Vec::new(), |mut abs_vec, expr| {
                abs_vec.push(trans_expr_inner(
                    expr, env, global_map, local_env, local_map,
                )?);
                Ok(abs_vec)
            })?;
            Ok(Abstract::Sum(abs_vec))
        }
        Expr::Pi(params, result) => {
            let mut pi_env = local_env.clone();
            let mut pi_map = local_map.clone();
            let mut pi_vec: Vec<Abstract> =
                params.iter().try_fold(Vec::new(), |pi_vec, param| {
                    trans_pi(env, global_map, &mut pi_env, &mut pi_map, pi_vec, param)
                })?;

            // fold from right
            pi_vec.reverse();
            Ok(pi_vec.iter().fold(
                trans_expr_inner(result, env, global_map, &pi_env, &pi_map)?,
                |pi_abs, param| Abstract::pi(param.clone(), pi_abs),
            ))
        }
    }
}

fn trans_pi(
    env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    pi_env: &mut AbstractGlobalEnv,
    pi_map: &mut NamedEnv_<DBI>,
    mut pi_vec: Vec<Abstract>,
    param: &Param,
) -> TCM<Vec<Abstract>> {
    // todo: handle implicit parameter
    // reference implementation:
    // https://github.com/owo-lang/OwO/blob/316e83cf532c447b03a7d210bbc3c4fc1409c861/src/type_check/elaborate.rs#L35-L64
    assert_eq!(param.kind, ParamKind::Explicit);
    let param_ty = trans_expr_inner(&param.ty, env, global_map, &pi_env, &pi_map)?;
    for name in param.names.clone() {
        let param_name = name.info.text;
        let param_dbi: DBI = pi_env.len();
        pi_env.insert(param_dbi, AbstractDecl::Sign(param_ty.clone()));
        pi_map.insert(param_name, param_dbi);
    }
    pi_vec.insert(pi_vec.len(), param_ty);
    Ok(pi_vec)
}
