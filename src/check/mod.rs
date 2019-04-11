use crate::syntax::core::{GlobalEnv, Term};
use crate::syntax::env::GlobalEnv_;
use crate::syntax::surf::ast::{Decl, Expr};
use std::collections::BTreeMap;

/// Type-Checking Error
pub enum TCE {
    SomeErr,
}

pub struct S {
    term_value: Option<Term>,
    term_type: Option<Term>,
}

pub type TCM<T> = Result<T, TCE>;

pub type TCS = GlobalEnv_<S>; // value and type

pub fn check_main(_decls: Vec<Decl>) -> TCM<TCS> {
    _decls.iter().fold(
        Result::Ok(BTreeMap::new()),
        |tcm: TCM<TCS>, decl| match tcm {
            Ok(env) => check_decl(env, decl),
            _ => tcm,
        },
    )
}

pub fn check_decl(tcs: TCS, decl: &Decl) -> TCM<TCS> {
    use crate::syntax::surf::ast::DeclKind::*;
    let kind = &decl.kind;
    match &kind {
        Sign => check(tcs, decl.body).map(|val| update_gamma(tcs, val)),
        Impl => check(tcs, decl.body).map(|val| update_env(tcs, val)),
    }
}

pub fn update_gamma(tcs: TCS, term: Term) -> TCS {
    unimplemented!()
}

pub fn update_env(tcs: TCS, term: Term) -> TCS {
    unimplemented!()
}

pub fn check(tcs: TCS, expr: Expr) -> TCM<Term> {
    unimplemented!()
}
