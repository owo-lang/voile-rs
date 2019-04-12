use crate::syntax::common::{SyntaxInfo, DBI};
use crate::syntax::core::{LocalEnv, Term};
use crate::syntax::env::GlobalEnv_;
use crate::syntax::surf::ast::{Decl, Expr};

/// Type-Checking Error.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TCE {
    CouldNotInfer(Term),
    NotImplemented,
}

/// Gamma item.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GammaItem {
    /// This is not a de Bruijn index, but it should be of the type `DBI`.
    /// It refers to its index in `TCS::env`.
    dbi: DBI,
    /// Because it's a name binding, there should be source code location.
    location: SyntaxInfo,
    /// The type of this name.
    r#type: Term,
}

/// Typing context.
pub type Gamma = GlobalEnv_<GammaItem>;

pub type TCM<T> = Result<T, TCE>;

#[derive(Debug, Clone, Default)]
pub struct TCS {
    /// Global+local value context.
    env: LocalEnv,
    /// This is not a de Bruijn index, but it should be of the type `DBI`.
    /// It represents the size of `env`.
    env_size: DBI,
    /// Global typing context.
    gamma: Gamma,
}

impl TCS {
    pub fn modify_env(mut self, f: impl FnOnce(LocalEnv) -> LocalEnv) -> Self {
        self.env = f(self.env);
        self
    }
}

/// Expr -> (well-typed) Term
pub fn check(tcs: TCS, expr: Expr) -> TCM<(TCS, Term)> {
    // TODO: it should have another param, representing the expected type.
    match expr {
        Expr::Type(_, level) => Ok((tcs, Term::Type(level))),
        _ => Err(TCE::NotImplemented),
    }
}

/// infer type of value
pub fn check_infer(tcs: TCS, value: Term) -> TCM<(TCS, Term)> {
    use crate::syntax::core::Term::*;
    match value {
        Type(level) => Ok((tcs, Type(level + 1))),
        _ => Err(TCE::CouldNotInfer(value)),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(tcs: TCS, subtype: Term, supertype: Term) -> TCM<(TCS, Term)> {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => {
            Ok((tcs, Type(super_level)))
        }
        _ => Err(TCE::NotImplemented),
    }
}

pub fn check_declarations(mut tcs: TCS, decls: Vec<Decl>) -> TCM<TCS> {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: Decl) -> TCM<TCS> {
    use crate::syntax::surf::ast::DeclKind::*;
    let name = decl.name;
    match decl.kind {
        Sign => {
            let (mut tcs, val) = check(tcs, decl.body)?;
            tcs.gamma.insert(
                name.info.text.clone(),
                GammaItem {
                    dbi: tcs.env_size,
                    location: name.info,
                    r#type: val,
                },
            );
            tcs = tcs.modify_env(|env| env.cons(Term::mock()));
            Ok(tcs)
        }
        Impl => {
            // TODO: TC properly
            let (mut tcs, val) = check(tcs, decl.body)?;
            // let ty = check_infer(tcs, val.clone())?;
            // TODO: Error handling for there's no corresponding item in Gamma
            let ctx_ty: GammaItem = tcs.gamma[&name.info.text].clone();
            // This is "replacing the type signature's corresponded value with a well-typed term"
            // TODO: Error handling, ditto
            tcs = tcs.modify_env(|env| env.substitute_at(ctx_ty.dbi, val).unwrap());
            Ok(tcs)
        }
    }
}
