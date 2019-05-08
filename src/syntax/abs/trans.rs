use std::collections::btree_map::BTreeMap;

use crate::check::monad::{TCE, TCM};
use crate::syntax::common::{ToSyntaxInfo, DBI};
use crate::syntax::surf::{Decl, DeclKind, Expr, Param};

use super::ast::*;

type NamedDbi = BTreeMap<String, DBI>;

pub fn trans_decls(decls: Vec<Decl>) -> TCM<Vec<AbsDecl>> {
    decls
        .iter()
        .try_fold(Default::default(), trans_one_decl)
        .map(|res| res.0)
}

type DeclTCS = (Vec<AbsDecl>, NamedDbi);

fn trans_one_decl((mut result, mut name_map): DeclTCS, decl: &Decl) -> TCM<DeclTCS> {
    let name = &decl.name;
    let abs = trans_expr(&decl.body, &result, &name_map)?;

    let dbi = *name_map
        .entry(name.info.text.clone())
        .or_insert_with(|| result.len());
    let original = if result.len() > dbi {
        Some(result.remove(dbi))
    } else {
        None
    };
    let modified = match (decl.kind, original) {
        (DeclKind::Sign, None) | (DeclKind::Sign, Some(AbsDecl::Sign(_))) => AbsDecl::Sign(abs),
        (DeclKind::Impl, None) | (DeclKind::Impl, Some(AbsDecl::Impl(_))) => AbsDecl::Impl(abs),
        (DeclKind::Sign, Some(AbsDecl::Impl(impl_abs)))
        | (DeclKind::Sign, Some(AbsDecl::Both(_, impl_abs))) => AbsDecl::Both(abs, impl_abs),
        (DeclKind::Impl, Some(AbsDecl::Sign(sign_abs)))
        | (DeclKind::Impl, Some(AbsDecl::Both(sign_abs, _))) => AbsDecl::Both(sign_abs, abs),
        // `AbsDecl::None` should not have other decls
        (_, Some(AbsDecl::None)) => AbsDecl::None,
    };
    result.insert(dbi, modified);
    Ok((result, name_map))
}

pub fn trans_expr(expr: &Expr, env: &[AbsDecl], map: &NamedDbi) -> TCM<Abs> {
    trans_expr_inner(expr, env, map, &[], &Default::default())
}

fn trans_expr_inner(
    expr: &Expr,
    env: &[AbsDecl],
    global_map: &NamedDbi,
    local_env: &[AbsDecl],
    local_map: &NamedDbi,
) -> TCM<Abs> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abs::Type(syntax.clone(), *level)),
        Expr::Var(ident) => {
            let name = &ident.info.text;
            if local_map.contains_key(name) {
                Ok(Abs::Local(ident.to_info(), local_map[name]))
            } else if global_map.contains_key(name) {
                Ok(Abs::Var(ident.to_info(), global_map[name]))
            } else {
                Err(TCE::LookUpFailed(ident.clone()))
            }
        }
        Expr::App(applied, app_vec) => app_vec.iter().try_fold(
            trans_expr_inner(applied, env, global_map, local_env, local_map)?,
            |result, each_expr| -> TCM<_> {
                let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                let info = result.to_info().merge(abs.to_info(), " ");
                Ok(Abs::app(info, result, abs))
            },
        ),
        // I really hope I can reuse the code with `App` here :(
        Expr::Pipe(startup, pipe_vec) => pipe_vec.iter().rev().try_fold(
            trans_expr_inner(startup, env, global_map, local_env, local_map)?,
            |result, each_expr| -> TCM<_> {
                let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                let info = result.to_info().merge(abs.to_info(), " ");
                Ok(Abs::app(info, result, abs))
            },
        ),
        Expr::Meta(ident) => Ok(Abs::Meta(ident.to_info())),
        Expr::Cons(ident) => Ok(Abs::Cons(ident.to_info())),
        Expr::ConsType(ident) => Ok(Abs::ConsType(ident.to_info())),
        Expr::Bot(ident) => Ok(Abs::Bot(ident.to_info())),
        Expr::Sum(first, variants) => {
            let abs_vec: Vec<Abs> = variants
                .iter()
                .map(|expr| trans_expr_inner(expr, env, global_map, local_env, local_map))
                .collect::<TCM<_>>()?;
            let first = trans_expr_inner(first, env, global_map, local_env, local_map)?;
            let info = abs_vec
                .iter()
                .fold(first.to_info(), |l, r| l.merge(r.to_info(), " | "));
            Ok(Abs::Sum(info, abs_vec))
        }
        // TODO: implement these three
        Expr::Tup(_first, _tup_vec) => unimplemented!(),
        Expr::Sig(_, _) => unimplemented!(),
        Expr::Lam(params, body) => {
            let mut local_env = local_env.to_vec();
            let mut local_map = local_map.clone();
            for param in params {
                local_env.insert(local_env.len(), AbsDecl::None);
                // todo: fix this
                local_map.insert(param.info.text.clone(), 0);
            }
            unimplemented!()
        }
        Expr::Pi(params, result) => {
            let mut pi_env = local_env.to_vec();
            let mut pi_map = local_map.clone();
            let pi_vec: Vec<Abs> = params.iter().try_fold(Vec::new(), |pi_vec, param| {
                trans_pi(env, global_map, &mut pi_env, &mut pi_map, pi_vec, param)
            })?;

            Ok(pi_vec.into_iter().rev().fold(
                trans_expr_inner(result, env, global_map, &pi_env, &pi_map)?,
                |pi_abs, param| {
                    let info = param.to_info().merge(pi_abs.to_info(), " -> ");
                    Abs::pi(info, param, pi_abs)
                },
            ))
        }
    }
}

fn trans_pi(
    env: &[AbsDecl],
    global_map: &NamedDbi,
    pi_env: &mut Vec<AbsDecl>,
    pi_map: &mut NamedDbi,
    mut pi_vec: Vec<Abs>,
    param: &Param,
) -> TCM<Vec<Abs>> {
    let param_ty = trans_expr_inner(&param.ty, env, global_map, &pi_env, &pi_map)?;
    for name in param.names.clone() {
        let param_name = name.info.clone();
        // These two are actually our assumption. Hope they're correct.
        assert_eq!(pi_env.len(), pi_map.len());
        // todo: implement shadowing?
        assert!(!pi_map.contains_key(&param_name.text));
        for (_name, dbi) in pi_map.iter_mut() {
            *dbi += 1;
        }
        pi_map.insert(param_name.text.clone(), 0);
        pi_env.insert(0, AbsDecl::Sign(param_ty.clone()));
    }
    pi_vec.push(param_ty);
    Ok(pi_vec)
}
