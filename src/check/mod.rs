use self::monad::{GammaItem, TermTCM, TCE, TCM, TCS};
use crate::syntax::core::Term;
use crate::syntax::surf::ast::{Decl, Expr};

pub mod monad;

/// Expr -> (well-typed) Term
pub fn check(tcs: TCS, expr: Expr, expected_type: Term) -> TermTCM {
    match (expr, expected_type) {
        // TODO: shouldn't type-check on surface syntax. Please do abstract syntax.
        (Expr::Type(_, lower), Term::Type(upper)) if upper > lower => Ok((tcs, Term::Type(lower))),
        _ => unimplemented!(),
    }
}

/// Check if an expression is a valid type expression
pub fn check_type(_tcs: TCS, _expr: Expr) -> TermTCM {
    unimplemented!()
}

/// infer type of value
pub fn check_infer(tcs: TCS, value: Term) -> TermTCM {
    use crate::syntax::core::Term::*;
    match value {
        Type(level) => Ok((tcs, Type(level + 1))),
        _ => Err(TCE::CouldNotInfer(value)),
    }
}

/// check if type1 is subtype of type2
pub fn check_subtype(tcs: TCS, subtype: Term, supertype: Term) -> TermTCM {
    use crate::syntax::core::Term::*;
    match (subtype, supertype) {
        (Type(sub_level), Type(super_level)) if sub_level <= super_level => {
            Ok((tcs, Type(super_level)))
        }
        _ => unimplemented!(),
    }
}

pub fn check_main(decls: Vec<Decl>) -> TCM {
    check_declarations(Default::default(), decls)
}

pub fn check_declarations(mut tcs: TCS, decls: Vec<Decl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: Decl) -> TCM {
    use crate::syntax::surf::ast::DeclKind::*;
    let name = decl.name;
    match decl.kind {
        Sign => {
            let (mut tcs, val) = check_type(tcs, decl.body)?;
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
            // TODO: Error handling for there's no corresponding item in Gamma
            let ctx_ty: GammaItem = tcs.gamma[&name.info.text].clone();
            let dbi = ctx_ty.dbi;
            let (mut tcs, val) = check(tcs, decl.body, ctx_ty.r#type)?;
            // This is "replacing the type signature's corresponded value with a well-typed term"
            // TODO: Error handling, ditto
            tcs = tcs.modify_env(|env| env.substitute_at(dbi, val).unwrap());
            Ok(tcs)
        }
    }
}
