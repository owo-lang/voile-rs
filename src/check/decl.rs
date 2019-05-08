use crate::syntax::abs::AbsDecl;

use super::expr::{check, check_type};
use super::monad::{TCM, TCS};
use crate::syntax::core::Term;

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: AbsDecl) -> TCM {
    match decl {
        AbsDecl::Both(_, sign_abs, _, impl_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let (sign, tcs) = check_type(tcs, sign_abs)?;
            let (val, mut tcs) = check(tcs, impl_abs, sign.ast.clone())?;

            tcs.gamma.push(sign);
            tcs.env.push(val);
            // Err(TCE::DbiOverflow(tcs.env.len(), new_dbi))
            Ok(tcs)
        }
        AbsDecl::Sign(_, sign_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let syntax_info = sign_abs.syntax_info().clone();
            let (val, mut tcs) = tcs.check_type(sign_abs)?;
            tcs.env.push(Term::axiom().into_info(syntax_info));
            tcs.gamma.push(val);
            // Give warning on axiom?
            Ok(tcs)
        }
        AbsDecl::Impl(_, impl_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let (inferred, tcs) = tcs.infer(impl_abs.clone())?;
            let (compiled, mut tcs) = tcs.unsafe_compile(impl_abs);
            tcs.env.push(compiled);
            tcs.gamma.push(inferred);
            Ok(tcs)
        }
    }
}

impl TCS {
    pub fn check_decls(self, decls: Vec<AbsDecl>) -> TCM {
        check_decls(self, decls)
    }

    pub fn check_decl(self, decl: AbsDecl) -> TCM {
        check_decl(self, decl)
    }
}
