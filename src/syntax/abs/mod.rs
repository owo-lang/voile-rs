use crate::check::monad::error::TCE;
use crate::check::monad::TCM;
use crate::syntax::common::{DtKind, Level, ParamKind, SyntaxInfo, DBI};
use crate::syntax::env::NamedEnv_;
use crate::syntax::surf::ast::{Decl, DeclKind, Expr};
use either::Either;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Abstract {
    Type(SyntaxInfo, Level),
    /// Bottom type
    Bot(SyntaxInfo),
    /// Global variable
    Var(SyntaxInfo, DBI),
    /// Local variable
    Local(SyntaxInfo, DBI),
    /// Construct call
    Cons(SyntaxInfo, Box<Self>),
    /// Construct call
    ConsType(SyntaxInfo, Box<Self>),
    /// Apply or Pipeline in surface
    App(Box<Self>, Box<Self>),
    /// Dependent Type type
    Dt(DtKind, Box<Self>, Box<Self>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
}

/// type signature and value in abstract syntax
#[derive(Debug, Clone)]
pub enum AbstractDecl {
    JustSign(Abstract),
    JustImpl(Abstract),
    Decl(Abstract, Abstract),
}

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
                let abs_decl = result.get(&dbi);
                result.insert(
                    dbi,
                    match (decl.kind, abs_decl.clone()) {
                        (DeclKind::Sign, None)
                        | (DeclKind::Sign, Some(AbstractDecl::JustSign(_))) => {
                            AbstractDecl::JustSign(abs)
                        }
                        (DeclKind::Impl, None)
                        | (DeclKind::Impl, Some(AbstractDecl::JustImpl(_))) => {
                            AbstractDecl::JustImpl(abs)
                        }
                        (DeclKind::Sign, Some(AbstractDecl::JustImpl(impl_abs)))
                        | (DeclKind::Sign, Some(AbstractDecl::Decl(_, impl_abs))) => {
                            AbstractDecl::Decl(abs, impl_abs.clone())
                        }
                        (DeclKind::Impl, Some(AbstractDecl::JustSign(sign_abs)))
                        | (DeclKind::Impl, Some(AbstractDecl::Decl(sign_abs, _))) => {
                            AbstractDecl::Decl(sign_abs.clone(), abs)
                        }
                    },
                );
                Ok((result, name_map))
            },
        )
        .map(|(result, _)| result)
}

pub fn trans_expr(expr: &Expr, env: &AbstractGlobalEnv, map: &NamedEnv_<DBI>) -> TCM<Abstract> {
    trans_expr_inner(expr, env, map, &BTreeMap::new(), &BTreeMap::new())
}

pub fn trans_expr_inner(
    expr: &Expr,
    env: &AbstractGlobalEnv,
    global_map: &NamedEnv_<DBI>,
    mut local_env: &AbstractGlobalEnv,
    mut local_map: &NamedEnv_<DBI>,
) -> TCM<Abstract> {
    match expr {
        Expr::Type(syntax, level) => Ok(Abstract::Type(syntax.clone(), *level)),
        Expr::Var(ident) => {
            let name = ident.info.text.clone();
            if local_map.contains_key(&name) {
                Ok(Abstract::Local(ident.info.clone(), local_map[&name]))
            } else if global_map.contains_key(&name) {
                Ok(Abstract::Var(ident.info.clone(), global_map[&name]))
            } else {
                Err(TCE::LookUpFailed(name))
            }
        }
        Expr::App(app_vec) => app_vec
            .iter()
            .fold(
                Ok(Either::Left(None)),
                |result: TCM<Either<Option<Abstract>, Abstract>>, each_expr| {
                    let abs = trans_expr_inner(each_expr, env, global_map, local_env, local_map)?;
                    Ok(match result? {
                        // First item in vec
                        Either::Left(None) => Either::Left(Some(abs)),
                        // Second or other, reduce to Right
                        Either::Left(Some(left_abs)) | Either::Right(left_abs) => {
                            Either::Right(Abstract::App(Box::new(left_abs), Box::new(abs)))
                        }
                    })
                },
            )
            .map(|e| match e {
                Either::Right(abs) => abs,
                // Any apply calls should have more than 2 items in the surface vec
                // so this should only have Right instance
                _ => unreachable!(),
            }),
        Expr::Pipe(pipe_vec) => {
            let mut app_vec = pipe_vec.clone();
            app_vec.reverse();
            trans_expr_inner(&Expr::App(app_vec), env, global_map, local_env, local_map)
        }
        Expr::Meta(_) => unimplemented!(),
        Expr::Cons(_) => unimplemented!(),
        Expr::ConsType(_) => unimplemented!(),
        Expr::Bot(ident) => Ok(Abstract::Bot(ident.info.clone())),
        Expr::Sum(_) => unimplemented!(),
        Expr::Pi(params, result) => {
            let mut pi_env = local_env.clone();
            let mut pi_map = local_map.clone();
            params
                .iter()
                .try_fold(
                    Either::Left(None),
                    |pi_abs: (Either<Option<Abstract>, Abstract>),
                     param|
                     -> TCM<Either<Option<Abstract>, Abstract>> {
                        // todo: handle implicit parameter
                        assert_eq!(param.kind, ParamKind::Explicit);
                        let param_ty: Abstract =
                            trans_expr_inner(&param.ty.clone(), env, global_map, &pi_env, &pi_map)?;
                        for name in param.names.clone() {
                            let param_name = name.info.text;
                            let param_dbi: DBI = local_env.len();
                            pi_env.insert(param_dbi, AbstractDecl::JustSign(param_ty.clone()));
                            pi_map.insert(param_name, param_dbi);
                        }
                        Ok(pi_abs)
                    },
                )
                .map_or_else(
                    |e| Err(e),
                    |pi_params| match pi_params {
                        Either::Left(Some(pi_params)) | Either::Right(pi_params) => {
                            let abs: Abstract =
                                trans_expr_inner(expr, env, global_map, &pi_env, &pi_map)?;
                            Ok(Abstract::Dt(DtKind::Pi, Box::new(pi_params), Box::new(abs)))
                        }
                        _ => unreachable!(),
                    },
                );
            unimplemented!()
        }
    }
}
