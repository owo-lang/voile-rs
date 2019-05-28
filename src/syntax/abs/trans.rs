use std::collections::btree_map::BTreeMap;

use crate::check::monad::{TCE, TCM};
use crate::syntax::common::{DtKind, ToSyntaxInfo, DBI};
use crate::syntax::surf::{Decl, DeclKind, Expr, Ident, Param};

use super::ast::*;

/// Key: global declaration name; Value: global declaration index.
type NamedDbi = BTreeMap<String, DBI>;

pub fn trans_decls(decls: Vec<Decl>) -> TCM<Vec<AbsDecl>> {
    trans_decls_contextual(Default::default(), decls).map(|tcs| tcs.0)
}

pub fn trans_decls_contextual(tcs: DeclTCS, decls: Vec<Decl>) -> TCM<DeclTCS> {
    decls.iter().try_fold(tcs, trans_one_decl)
}

pub type DeclTCS = (Vec<AbsDecl>, NamedDbi);

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
    local_env: &[Name],
    local_map: &NamedDbi,
) -> TCM<Abs> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abs::Type(syntax.clone(), *level)),
        Expr::Var(ident) => {
            let name = &ident.info.text;
            if local_map.contains_key(name) {
                let dbi = local_map[name];
                Ok(Abs::Local(ident.to_info(), local_env[dbi], dbi))
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
        Expr::Variant(ident) => Ok(Abs::Variant(ident.to_info())),
        Expr::Bot(ident) => Ok(Abs::Bot(ident.to_info())),
        Expr::Sum(first, variants) => {
            let mut abs_vec: Vec<Abs> = variants
                .iter()
                .map(|expr| trans_expr_inner(expr, env, global_map, local_env, local_map))
                .collect::<TCM<_>>()?;
            let first = trans_expr_inner(first, env, global_map, local_env, local_map)?;
            let info = abs_vec
                .iter()
                .fold(first.to_info(), |l, r| l.merge(r.to_info(), " | "));
            abs_vec.push(first);
            Ok(Abs::Sum(info, abs_vec))
        }
        Expr::Tup(first, tup_vec) => tup_vec.iter().try_fold(
            trans_expr_inner(first, env, global_map, local_env, local_map)?,
            |pair, expr| {
                let abs = trans_expr_inner(expr, env, global_map, local_env, local_map)?;
                Ok(Abs::pair(abs.to_info(), pair, abs))
            },
        ),
        Expr::Sig(initial, last) => trans_dependent_type(
            env,
            global_map,
            local_env,
            local_map,
            initial,
            &*last,
            DtKind::Sigma,
        ),
        Expr::Lam(info, params, body) => {
            let mut local_env = local_env.to_vec();
            local_env.reserve_exact(local_env.len() + params.len() + 1);
            let mut local_map = local_map.clone();
            let mut names = Vec::with_capacity(params.len());
            introduce_abstractions(params, &mut local_env, &mut local_map, &mut names);
            Ok(params.iter().rev().fold(
                trans_expr_inner(body, env, global_map, &local_env, &local_map)?,
                |lam_abs, param| {
                    let pop_empty = "The stack `names` is empty. Please report this as a bug.";
                    let name = names.pop().expect(pop_empty);
                    Abs::lam(info.clone(), param.to_info(), name, lam_abs)
                },
            ))
        }
        Expr::Pi(params, result) => trans_dependent_type(
            env,
            global_map,
            local_env,
            local_map,
            params,
            &*result,
            DtKind::Pi,
        ),
    }
}

fn introduce_abstractions(
    params: &[Ident],
    local_env: &mut Vec<Name>,
    local_map: &mut NamedDbi,
    names: &mut Vec<Name>,
) {
    for param in params {
        let shadowing = local_map.get(&param.info.text).cloned();
        for (_name, dbi) in local_map.iter_mut() {
            let dbi_value = *dbi;
            *dbi += 1;
            if shadowing == Some(dbi_value) {
                local_env.remove(dbi_value);
            }
        }
        local_map.insert(param.info.text.clone(), 0);
        let new_name = Name::default();
        local_env.insert(0, new_name);
        names.push(new_name);
    }
}

fn trans_dependent_type(
    env: &[AbsDecl],
    glob: &NamedDbi,
    local_env: &[Name],
    local_map: &NamedDbi,
    params: &[Param],
    result: &Expr,
    kind: DtKind,
) -> TCM<Abs> {
    let mut pi = local_env.to_vec();
    let mut pi_map = local_map.clone();
    let mut names = Vec::with_capacity(params.len());
    let pi_vec: Vec<Abs> = params.iter().try_fold(Vec::new(), |pi_vec, param| {
        introduce_telescope(env, glob, &mut pi, &mut pi_map, &mut names, pi_vec, param)
    })?;

    Ok(pi_vec.into_iter().rev().fold(
        trans_expr_inner(result, env, glob, &pi, &pi_map)?,
        |pi_abs, param| {
            let info = param.to_info().merge(pi_abs.to_info(), " -> ");
            let pop_empty = "The stack `names` is empty. Please report this as a bug.";
            Abs::dependent_type(info, kind, names.pop().expect(pop_empty), param, pi_abs)
        },
    ))
}

fn introduce_telescope(
    env: &[AbsDecl],
    global_map: &NamedDbi,
    dt_env: &mut Vec<Name>,
    dt_map: &mut NamedDbi,
    names: &mut Vec<Name>,
    mut dt_vec: Vec<Abs>,
    param: &Param,
) -> TCM<Vec<Abs>> {
    let param_ty = trans_expr_inner(&param.ty, env, global_map, &dt_env, &dt_map)?;
    for name in &param.names {
        let param_name = name.info.text.clone();
        // These two are actually our assumption. Hope they're correct.
        assert_eq!(dt_env.len(), dt_map.len());
        // let shadowing = dt_map.get(&param_name).cloned();
        dt_map.iter_mut().for_each(|(_name, dbi)| *dbi += 1);
        /*
        let dbi_value = *dbi;
        if shadowing == Some(dbi_value) {
            // just remove the DBI map, for there still need to be a place holder in `Vec`
            dt_env.remove(dbi_value);
            // this is not necessary for dt_map will be updated below
            // *dbi = 0;
        }
        */
        dt_map.insert(param_name, 0);
        let new_name = Name::default();
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push(param_ty.clone());
    }
    if param.names.is_empty() {
        let new_name = Name::default();
        dt_map.iter_mut().for_each(|(_name, dbi)| *dbi += 1);
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push(param_ty);
    }
    Ok(dt_vec)
}
