use crate::check::monad::error::TCE;
use crate::check::monad::TCM;
use crate::syntax::common::{DtKind, Level, DBI};
use crate::syntax::env::NamedEnv_;
use crate::syntax::surf::ast::{Decl, DeclKind, Expr, Ident};
use std::collections::BTreeMap;
use std::process::id;

#[derive(Debug, Clone)]
pub enum Abstract {
    Type(Ident, Level),
    /// Bottom type
    Bot(Ident),
    /// Global variable
    Var(Ident, DBI),
    /// Local variable
    Local(Ident, DBI),
    /// Construct call
    Cons(Ident, Box<Abstract>),
    /// Apply or Pipeline in surface
    App(Ident, Box<Self>, Box<Self>),
    /// Dependent Type type
    Dt(Ident, DtKind, Box<Self>, Box<Self>),
    Pair(Ident, Box<Abstract>, Box<Abstract>),
    Fst(Ident, Box<Abstract>),
    Snd(Ident, Box<Abstract>),
}

/// type signature and value
pub type AbstractDecl = (Option<Abstract>, Option<Abstract>);

pub type AbstractGlobalEnv = VecDbiEnv_<AbstractDecl>;

pub type VecDbiEnv_<T> = BTreeMap<DBI, T>;

/// todo: replace to proper location
pub fn trans(decls: Vec<Decl>) -> TCM<AbstractGlobalEnv> {
    decls
        .iter()
        .fold(
            Ok(Default::default()),
            |result: TCM<(AbstractGlobalEnv, NamedEnv_<DBI>)>, decl| {
                let name = decl.name.clone();
                let (mut result, mut name_map) = result?;

                let abs = trans_expr(&decl.body, &result, &name_map)?;

                let dbi = name_map
                    .entry(name.info.text)
                    .or_insert(result.len())
                    .clone();
                if !result.contains_key(&dbi) {
                    result.insert(dbi, (None, None));
                }
                result.entry(dbi).and_modify(|abs_tuple| {
                    match decl.kind {
                        DeclKind::Sign => abs_tuple.0 = Some(abs),
                        DeclKind::Impl => abs_tuple.1 = Some(abs),
                    };
                });
                Ok((result, name_map))
            },
        )
        .map(|(result, _)| result)
}

pub fn trans_expr(_expr: &Expr, _env: &AbstractGlobalEnv, _map: &NamedEnv_<DBI>) -> TCM<Abstract> {
    trans_expr_inner(_expr, _env, _map, Default::default(), &BTreeMap::new())
}

pub fn trans_expr_inner(
    _expr: &Expr,
    _env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    _local_env: AbstractGlobalEnv,
    local_map: &NamedEnv_<DBI>,
) -> TCM<Abstract> {
    match _expr {
        Expr::Type(syntax, level) => Ok(Abstract::Type(
            Ident {
                info: syntax.clone(),
            },
            *level,
        )),
        Expr::Var(ident) => {
            let name = ident.info.text.clone();
            if local_map.contains_key(&name) {
                Ok(Abstract::Local(ident.clone(), local_map[&name]))
            } else if global_map.contains_key(&name) {
                Ok(Abstract::Var(ident.clone(), global_map[&name]))
            } else {
                Err(TCE::LookUpFailed(name))
            }
        }
        Expr::Meta(_) => unimplemented!(),
        _ => unimplemented!(),
    }
}
