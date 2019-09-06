use std::collections::BTreeMap;

use voile_util::loc::*;
use voile_util::meta::MI;
use voile_util::tags::{PiSig::*, *};
use voile_util::uid::*;

use crate::check::monad::{TCE, TCM};
use crate::syntax::surf::{Decl, DeclKind, Expr, Param};

use super::ast::*;

/// Key: global declaration name; Value: global declaration index.
type GlobCtx = BTreeMap<String, GI>;

/// Key: local declaration name; Value: de-bruijn indices.
type LocalCtx = BTreeMap<String, (DBI, Plicit)>;

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
            return Err(TCE::ReDefine(decl.name.loc, thing.loc()));
        }
        // Re-defining something, should give error
        (_, Some(AbsDecl::Impl(thing, ..))) | (_, Some(AbsDecl::Decl(thing))) => {
            return Err(TCE::ReDefine(decl.name.loc, thing.loc()));
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
    let mut recursion =
        |e: Expr| trans_expr_inner(e, meta_count, env, global_map, local_env, local_map);
    let map_labels =
        |Labelled { expr, label }| recursion(expr).map(|expr| Labelled { label, expr });
    match expr {
        Expr::Type(syntax, level) => Ok(Abs::Type(syntax, level)),
        Expr::Var(ident) => {
            let name = &ident.text;
            if local_map.contains_key(name) {
                let (dbi, _) = local_map[name];
                Ok(Abs::Var(ident.clone(), local_env[dbi.0], dbi))
            } else if global_map.contains_key(name) {
                Ok(Abs::Ref(ident.clone(), global_map[name]))
            } else {
                Err(TCE::LookUpFailed(ident.clone()))
            }
        }
        Expr::App(app_vec) => Ok(app_vec.try_map(recursion)?.fold1(|result: Abs, abs: Abs| {
            Abs::app(merge_info(&result, &abs), result, Plicit::Ex, abs)
        })),
        // I really hope I can reuse the code with `App` here :(
        Expr::Pipe(pipe_vec) => Ok(pipe_vec
            .try_map(recursion)?
            .rev_fold1(|result, abs| Abs::app(merge_info(&result, &abs), result, Plicit::Ex, abs))),
        Expr::Meta(ident) => {
            let ret = Ok(Abs::Meta(ident.clone(), *meta_count));
            *meta_count += 1;
            ret
        }
        Expr::Cons(ident) => Ok(Abs::Cons(ident.clone())),
        // TODO: check uniqueness?
        Expr::RowKind(info, kind, labels) => Ok(Abs::RowKind(info, kind, labels)),
        Expr::Proj(expr, projections) => Ok(projections.fold(recursion(*expr)?, |abs, label| {
            Abs::proj(merge_info(&abs, &label), abs, label)
        })),
        Expr::RowPoly(info, kind, labels, rest) => {
            let labels: Result<_, _> = labels.into_iter().map(map_labels).collect();
            let rest = rest.map(|e| recursion(*e)).transpose()?;
            Ok(Abs::row_polymorphic_type(info, kind, labels?, rest))
        }
        Expr::Rec(info, fields, rest) => {
            let labels: Result<_, _> = fields.into_iter().map(map_labels).collect();
            let rest = rest.map(|e| recursion(*e)).transpose()?;
            Ok(Abs::record(info, labels?, rest))
        }
        Expr::Tup(tup_vec) => Ok(tup_vec
            .try_map(recursion)?
            .fold1(|pair, abs| Abs::pair(abs.loc(), pair, abs))),
        Expr::Sig(initial, last) => trans_dependent_type(
            meta_count, env, global_map, local_env, local_map, initial, *last, Sigma,
        ),
        Expr::Cases(label, binding, body, or) => {
            let or = trans_expr_inner(*or, meta_count, env, global_map, local_env, local_map)?;
            let mut local = local_env.to_vec();
            local.reserve_exact(local.len() + 1);
            let mut local_map = local_map.clone();
            let mut names = Vec::with_capacity(1);
            introduce_abstractions(&[binding.clone()], &mut local, &mut local_map, &mut names);
            let body = trans_expr_inner(*body, meta_count, env, global_map, &local, &local_map)?;
            Ok(Abs::case_or(label, binding, names[0], body, or))
        }
        Expr::Whatever(info) => Ok(Abs::Whatever(info)),
        Expr::Lam(info, params, body) => {
            let mut local = local_env.to_vec();
            local.reserve_exact(local.len() + params.len() + 1);
            let mut local_map = local_map.clone();
            let mut names = Vec::with_capacity(params.len());
            introduce_abstractions(&params, &mut local, &mut local_map, &mut names);
            let body = trans_expr_inner(*body, meta_count, env, global_map, &local, &local_map)?;
            Ok(params.into_iter().rev().fold(body, |lam_abs, param| {
                let pop_empty = "The stack `names` is empty. Please report this as a bug.";
                let name = names.pop().expect(pop_empty);
                Abs::lam(info.clone(), param.clone(), name, lam_abs)
            }))
        }
        Expr::Pi(params, result) => trans_dependent_type(
            meta_count, env, global_map, local_env, local_map, params, *result, Pi,
        ),
        Expr::Lift(info, levels, inner) => Ok(Abs::lift(info, levels, recursion(*inner)?)),
    }
}

fn introduce_abstractions(
    params: &[Ident],
    local_env: &mut Vec<UID>,
    local_map: &mut LocalCtx,
    names: &mut Vec<UID>,
) {
    for param in params {
        let shadowing = local_map.get(&param.text).cloned().map(|(dbi, _)| dbi);
        for (_name, (dbi, _)) in local_map.iter_mut() {
            let dbi_value = *dbi;
            *dbi += 1;
            if shadowing == Some(dbi_value) {
                local_env.remove(dbi_value.0);
            }
        }
        local_map.insert(param.text.clone(), (Default::default(), Plicit::Ex));
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
    kind: PiSig,
) -> TCM<Abs> {
    let mut pi_env = local_env.to_vec();
    let mut pi_map = local_map.clone();
    let mut names = Vec::with_capacity(params.len());
    let pi_initial = Vec::with_capacity(params.len());
    let pi_vec: Vec<(Abs, Plicit)> = params.into_iter().try_fold(pi_initial, |pi_vec, param| {
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
        |pi_abs, (param, plicit)| {
            let info = param.loc() + pi_abs.loc();
            let pop_empty = "The stack `names` is empty. Please report this as a bug.";
            Abs::dependent_type(
                info,
                kind,
                names.pop().expect(pop_empty),
                plicit,
                param,
                pi_abs,
            )
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
    mut dt_vec: Vec<(Abs, Plicit)>,
    param: Param,
) -> TCM<Vec<(Abs, Plicit)>> {
    let param_ty = trans_expr_inner(param.ty, meta_count, env, global_map, &dt_env, &dt_map)?;
    for name in &param.names {
        let param_name = name.text.clone();
        // These two are actually our assumption. Hope they're correct.
        assert_eq!(dt_env.len(), dt_map.len());
        // let shadowing = dt_map.get(&param_name).cloned();
        dt_map.iter_mut().for_each(|(_name, (dbi, _))| *dbi += 1);
        /*
        let dbi_value = *dbi;
        if shadowing == Some(dbi_value) {
            // just remove the DBI map, for there still need to be a place holder in `Vec`
            dt_env.remove(dbi_value);
            // this is not necessary for dt_map will be updated below
            // *dbi = 0;
        }
        */
        dt_map.insert(param_name, (Default::default(), param.plicit));
        let new_name = unsafe { next_uid() };
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push((param_ty.clone(), param.plicit));
    }
    if param.names.is_empty() {
        let new_name = unsafe { next_uid() };
        dt_map.iter_mut().for_each(|(_name, (dbi, _))| *dbi += 1);
        dt_env.insert(0, new_name);
        names.push(new_name);
        dt_vec.push((param_ty, param.plicit));
    }

    Ok(dt_vec)
}
