use std::mem::swap;

use crate::syntax::abs::AbsDecl;
use crate::syntax::common::{ToSyntaxInfo, DBI};
use crate::syntax::core::{AxiomEx, Val, ValInfo};

use super::monad::{TCM, TCS};

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = tcs.check_decl(decl)?;
    }
    Ok(tcs)
}

fn require_local_emptiness(tcs: &TCS) {
    debug_assert!(tcs.local_env.is_empty());
    debug_assert!(tcs.local_gamma.is_empty());
}

fn check_decl(tcs: TCS, decl: AbsDecl) -> TCM {
    match decl {
        AbsDecl::Impl(impl_abs, sign_dbi) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let sign = tcs.glob_type(sign_dbi);
            let sign_cloned = sign.ast.clone();
            let (val_fake, mut tcs) = tcs.check(&impl_abs, &sign_cloned)?;
            // We generate axioms for lambda parameters during type-checking.
            // Now it's time to change them back to `var` references.
            let val = val_fake.map_ast(|ast| ast.generated_to_var());

            tcs.env[sign_dbi] = val;

            fn unimplemented_to_glob(v: &mut [ValInfo], i: DBI) {
                let mut placeholder = Default::default();
                swap(&mut v[i], &mut placeholder);
                placeholder = placeholder.map_ast(|ast| ast.unimplemented_to_glob());
                swap(&mut placeholder, &mut v[i]);
            }
            // Every references to me are now actually valid (they were axioms before),
            // replace them with a global reference.
            for i in sign_dbi..tcs.glob_size() {
                unimplemented_to_glob(tcs.env.as_mut_slice(), i);
            }
            for i in sign_dbi + 1..tcs.glob_size() {
                unimplemented_to_glob(tcs.gamma.as_mut_slice(), i);
            }

            require_local_emptiness(&tcs);
            // Err(TCE::DbiOverflow(tcs.env.len(), new_dbi))
            Ok(tcs)
        }
        AbsDecl::Sign(sign_abs, self_index) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let syntax_info = sign_abs.syntax_info();
            let (sign_fake, mut tcs) = tcs.check_type(&sign_abs)?;
            let sign = sign_fake.map_ast(|ast| ast.generated_to_var());
            let val_info = Val::fresh_unimplemented(self_index).into_info(syntax_info);
            tcs.env.push(val_info);
            tcs.gamma.push(sign);

            require_local_emptiness(&tcs);
            // Give warning on axiom?
            Ok(tcs)
        }
        AbsDecl::Decl(impl_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let (inferred, tcs) = tcs.infer(&impl_abs)?;
            let (compiled, mut tcs) = tcs.evaluate(impl_abs);
            let compiled = compiled.map_ast(|ast| ast.generated_to_var());
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
    pub fn check_decl(self, decl: AbsDecl) -> TCM {
        check_decl(self, decl)
    }
}
