use crate::syntax::core::Term;
use crate::syntax::env::GlobalEnv_;
use crate::syntax::surf::ast::{Decl, Expr};
use std::collections::BTreeMap;

/// Type-Checking Error
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TCE {
    CouldNotInfer(Term),
    NotImplemented,
}

/// env and gamma unit, todo: rename it
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct S {
    term_value: Option<Term>,
    term_type: Option<Term>,
}

pub type TCM<T> = Result<T, TCE>;

pub type TCS = GlobalEnv_<S>; // value and type

/// Expr -> (well-typed) Term
pub fn check(tcs: TCS, expr: Expr) -> TCM<(TCS, Term)> {
    match expr {
        Expr::Type(_, level) => Ok((tcs, Term::Type(level))),
        _ => Err(TCE::NotImplemented),
    }
}

/// infer type of value
pub fn check_infer(_tcs: TCS, value: Term) -> TCM<Term> {
    use crate::syntax::core::Term::*;
    match value {
        Type(level) => Ok(Type(level + 1)),
        _ => Err(TCE::CouldNotInfer(value)),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(_tcs: TCS, subtype: Term, supertype: Term) -> TCM<Term> {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => Ok(Type(super_level)),
        _ => Err(TCE::NotImplemented),
    }
}

pub fn check_main(decls: Vec<Decl>) -> TCM<TCS> {
    decls.iter().fold(
        Result::Ok(BTreeMap::new()),
        |tcm: TCM<TCS>, decl| match tcm {
            Ok(env) => check_decl(env, decl.clone()),
            _ => tcm,
        },
    )
}

pub fn check_decl(tcs: TCS, decl: Decl) -> TCM<TCS> {
    use crate::syntax::surf::ast::DeclKind::*;
    let name = decl.name.clone();
    match decl.kind {
        Sign => {
            let (tcs, val) = check(tcs, decl.body)?;
            let mut tcs = tcs.clone();
            tcs.insert(
                name.info.text,
                S {
                    term_value: None,
                    term_type: Some(val),
                },
            );
            Ok(tcs)
        }
        Impl => {
            let (tcs, val) = check(tcs, decl.body)?;
            let ty = check_infer(tcs.clone(), val.clone())?;
            let checked_ty = match tcs.get(&name.info.text).cloned() {
                Some(S {
                    term_type: Some(sig_ty),
                    ..
                }) => check_subtype(tcs.clone(), ty, sig_ty),
                _ => Ok(ty),
            }?;
            let mut tcs = tcs.clone();
            tcs.insert(
                name.info.text,
                S {
                    term_value: Some(val),
                    term_type: Some(checked_ty),
                },
            );
            Ok(tcs)
        }
    }
}
