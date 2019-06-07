use crate::syntax::abs::AbsDecl;
use crate::syntax::common::{ToSyntaxInfo, DBI};
use crate::syntax::core::Val;

use super::monad::{TCM, TCS};

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    for (index, decl) in decls.into_iter().enumerate() {
        tcs = tcs.check_decl(decl, index)?;
    }
    Ok(tcs)
}

fn require_local_emptiness(tcs: &TCS) {
    debug_assert!(tcs.local_env.is_empty());
    debug_assert!(tcs.local_gamma.is_empty());
}

fn check_decl(tcs: TCS, decl: AbsDecl, index: DBI) -> TCM {
    match decl {
        AbsDecl::Impl(impl_abs, sign_dbi) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let sign = tcs.glob_val(sign_dbi);
            let sign_cloned = sign.ast.clone();
            let (val_fake, mut tcs) = tcs.check(&impl_abs, &sign_cloned)?;
            let val = val_fake.map_ast(|ast| ast.axiom_to_var());

            tcs.env[sign_dbi] = val;
            require_local_emptiness(&tcs);
            // Err(TCE::DbiOverflow(tcs.env.len(), new_dbi))
            Ok(tcs)
        }
        AbsDecl::Sign(sign_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let syntax_info = sign_abs.syntax_info();
            let (sign_fake, mut tcs) = tcs.check_type(&sign_abs)?;
            let sign = sign_fake.map_ast(|ast| ast.axiom_to_var());
            tcs.env.push(Val::axiom().into_info(syntax_info));
            tcs.gamma.push(sign);

            require_local_emptiness(&tcs);
            // Give warning on axiom?
            Ok(tcs)
        }
        AbsDecl::Decl(impl_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let (inferred, tcs) = tcs.infer(&impl_abs)?;
            let (compiled, mut tcs) = tcs.evaluate(impl_abs);
            let compiled = compiled.map_ast(|ast| ast.axiom_to_var());
            tcs.env.push(compiled);
            tcs.gamma.push(inferred);

            require_local_emptiness(&tcs);
            Ok(tcs)
        }
    }
}

impl TCS {
    #[inline]
    pub fn check_decls(self, decls: Vec<AbsDecl>) -> TCM {
        check_decls(self, decls)
    }

    #[inline]
    pub fn check_decl(self, decl: AbsDecl, index: DBI) -> TCM {
        check_decl(self, decl, index)
    }
}
