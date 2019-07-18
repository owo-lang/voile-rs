use std::collections::btree_map::BTreeMap;

use crate::check::monad::{TCE, TCM};
use crate::syntax::common::{DtKind::*, *};
use crate::syntax::surf::{Decl, DeclKind, Expr, Param};

use super::ast::*;

/// Key: global declaration name; Value: global declaration index.
type GlobCtx = BTreeMap<String, GI>;

/// Key: local declaration name; Value: de-bruijn indices.
type LocalCtx = BTreeMap<String, DBI>;

pub fn trans_decls(decls: Vec<Decl>) -> TCM<Vec<AbsDecl>> {
    trans_decls_contextual(Default::default(), decls).map(|tcs| tcs.decls)
}

pub fn trans_decls_contextual(tcs: TransState, decls: Vec<Decl>) -> TCM<TransState> {
    decls.into_iter().try_fold(tcs, trans_one_decl)
}

/// Translation state.
#[derive(Debug, Clone, Default)]
pub struct TransState {
    pub decls: Vec<AbsDecl>,
    /// The index of the $n_{th}$ valid declaration
    /// (`Impl`s are not counted as valid declarations because they're just
    /// attachments to existing declarations).
    pub signature_indices: Vec<DBI>,
    pub context_mapping: GlobCtx,
    pub decl_count: GI,
    pub meta_count: MI,
}

fn trans_one_decl(mut tcs: TransState, decl: Decl) -> TCM<TransState> {
    let abs = trans_expr(
        decl.body,
        &tcs.decls,
        &mut tcs.meta_count,
        &tcs.context_mapping,
    )?;

    let decl_total = tcs.decl_count;
    let dbi = *tcs
        .context_mapping
        .entry(decl.name.text.clone())
        .or_insert_with(|| decl_total);
    let original = if decl_total > dbi {
        Some(&tcs.decls[tcs.signature_indices[dbi.0].0])
    } else {
        None
    };
    let modified = match (decl.kind, original) {
        (DeclKind::Sign, None) => {
            let abs = AbsDecl::Sign(abs, tcs.decl_count);
            tcs.signature_indices.push(DBI(tcs.decls.len()));
            tcs.decl_count += 1;
            abs
        }
        // Re-type-signaturing something, should give error
        (DeclKind::Sign, Some(thing)) => {
            return Err(TCE::ReDefine(decl.name.info, thing.syntax_info()))
        }
        // Re-defining something, should give error
        (_, Some(AbsDecl::Impl(thing, ..))) | (_, Some(AbsDecl::Decl(thing))) => {
            return Err(TCE::ReDefine(decl.name.info, thing.syntax_info()))
        }
        (DeclKind::Impl, None) => {
            tcs.decl_count += 1;
            tcs.signature_indices.push(DBI(tcs.decls.len()));
            AbsDecl::Decl(abs)
        }
        (DeclKind::Impl, Some(AbsDecl::Sign(_, dbi))) => AbsDecl::Impl(abs, *dbi),
    };
    tcs.decls.push(modified);
    Ok(tcs)
}

pub fn trans_expr(expr: Expr, env: &[AbsDecl], meta_count: &mut MI, map: &GlobCtx) -> TCM<Abs> {
    trans_expr_inner(expr, meta_count, env, map, &[], &Default::default())
}

fn trans_expr_inner(
    expr: Expr,
    meta_count: &mut MI,
    env: &[AbsDecl],
    global_map: &GlobCtx,
    local_env: &[UID],
    local_map: &LocalCtx,
) -> TCM<Abs> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abs::Type(syntax, level)),
        Expr::Var(ident) => {
            let name = &ident.text;
            if local_map.contains_key(name) {
                let dbi = local_map[name];
                Ok(Abs::Var(ident.clone(), local_env[dbi.0], dbi))
            } else if global_map.contains_key(name) {
                Ok(Abs::Ref(ident.clone(), global_map[name]))
            } else {
                Err(TCE::LookUpFailed(ident.clone()))
            }
        }
        Expr::App(app_vec) => Ok(app_vec
            .try_map(|e| trans_expr_inner(e, meta_count, env, global_map, local_env, local_map))?
            .fold1(|result: Abs, abs: Abs| {
                let info = result.syntax_info() + abs.syntax_info();
                Abs::app(info, result, abs)
            })),
        // I really hope I can reuse the code with `App` here :(
        Expr::Pipe(pipe_vec) => Ok(pipe_vec
            .try_map(|e| trans_expr_inner(e, meta_count, env, global_map, local_env, local_map))?
            .rev_fold1(|result, abs| {
                let info = result.syntax_info() + abs.syntax_info();
                Abs::app(info, result, abs)
            })),
        Expr::Meta(ident) => {
            let ret = Ok(Abs::Meta(ident.clone(), *meta_count));
            *meta_count += 1;
            ret
        }
        Expr::Cons(ident) => Ok(Abs::Cons(ident.clone())),
        Expr::Bot(info) => Ok(Abs::Bot(info)),
        Expr::RowPoly(labels, kind, variants) => unimplemented!(),
        Expr::Tup(tup_vec) => Ok(tup_vec
            .try_map(|e| trans_expr_inner(e, meta_count, env, global_map, local_env, local_map))?
            .fold1(|pair, abs| Abs::pair(abs.syntax_info(), pair, abs))),
        Expr::Sig(initial, last) => trans_dependent_type(
            meta_count, env, global_map, local_env, local_map, initial, *last, Sigma,
        ),
        Expr::Lam(info, params, body) => {
            let mut local_env = local_env.to_vec();
            local_env.reserve_exact(local_env.len() + params.len() + 1);
            let mut local_map = local_map.clone();
            let mut names = Vec::with_capacity(params.len());
            introduce_abstractions(&params, &mut local_env, &mut local_map, &mut names);
            let body =
                trans_expr_inner(*body, meta_count, env, global_map, &local_env, &local_map)?;
            Ok(params.into_iter().rev().fold(body, |lam_abs, param| {
                let pop_empty = "The stack `names` is empty. Please report this as a bug.";
                let name = names.pop().expect(pop_empty);
                Abs::lam(info, param.clone(), name, lam_abs)
            }))
        }
        Expr::Pi(params, result) => trans_dependent_type(
            meta_count, env, global_map, local_env, local_map, params, *result, Pi,
        ),
        Expr::Lift(info, levels, inner) => {
            let inner =
                trans_expr_inner(*inner, meta_count, env, global_map, local_env, local_map)?;
            Ok(Abs::lift(info, levels, inner))
        }
    }
}

fn introduce_abstractions(
    params: &[Ident],
    local_env: &mut Vec<UID>,
    local_map: &mut LocalCtx,
    names: &mut Vec<UID>,
) {
    for param in params {
        let shadowing = local_map.get(&param.text).cloned();
        for (_name, dbi) in local_map.iter_mut() {
            let dbi_value = *dbi;
            *dbi += 1;
            if shadowing == Some(dbi_value) {
                local_env.remove(dbi_value.0);
            }
        }
        local_map.insert(param.text.clone(), Default::default());
        let new_name = unsafe { next_uid() };
        local_env.insert(0, new_name);
        names.push(new_name);
    }
}

fn trans_dependent_type(
    meta_count: &mut MI,
    env: &[AbsDecl],
    global_map: &GlobCtx,
    local_env: &[UID],
    local_map: &LocalCtx,
    params: Vec<Param>,
    result: Expr,
    kind: DtKind,
) -> TCM<Abs> {
    let mut pi_env = local_env.to_vec();
    let mut pi_map = local_map.clone();
    let mut names = Vec::with_capacity(params.len());
    let pi_vec: Vec<Abs> = params.into_iter().try_fold(Vec::new(), |pi_vec, param| {
        introduce_telescope(
            meta_count,
            env,
            global_map,
            &mut pi_env,
            &mut pi_map,
            &mut names,
            pi_vec,
            param,
        )
    })?;

    Ok(pi_vec.into_iter().rev().fold(
        trans_expr_inner(result, meta_count, env, global_map, &pi_env, &pi_map)?,
        |pi_abs, param| {
            let info = param.syntax_info() + pi_abs.syntax_info();
            let pop_empty = "The stack `names` is empty. Please report this as a bug.";
            Abs::dependent_type(info, kind, names.pop().expect(pop_empty), param, pi_abs)
        },
    ))
}

fn introduce_telescope(
    meta_count: &mut MI,
    env: &[AbsDecl],
    global_map: &GlobCtx,
    dt_env: &mut Vec<UID>,
    dt_map: &mut LocalCtx,
    names: &mut Vec<UID>,
    mut dt_vec: Vec<Abs>,
    param: Param,
) -> TCM<Vec<Abs>> {
    let param_ty = trans_expr_inner(param.ty, meta_count, env, global_map, &dt_env, &dt_map)?;
    for name in &param.names {
        let param_name = name.text.clone();
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
        dt_map.insert(param_name, Default::default());
        let new_name = unsafe { next_uid() };
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push(param_ty.clone());
    }
    if param.names.is_empty() {
        let new_name = unsafe { next_uid() };
        dt_map.iter_mut().for_each(|(_name, dbi)| *dbi += 1);
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push(param_ty);
    }
    Ok(dt_vec)
}
