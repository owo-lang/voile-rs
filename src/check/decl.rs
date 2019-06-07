use crate::syntax::abs::AbsDecl;
use crate::syntax::common::ToSyntaxInfo;
use crate::syntax::core::Val;

use super::monad::{TCM, TCS};

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    // `AbsDecl::Impl` uses an index to indicate which signature it's corresponding to.
    // However, in `check_decl`, we **replace** an existing definition with type-check `Impl`
    // (instead of adding it to the context, like `Sign` or `Decl`).
    // This causes a difference between the actual index of the definition and the index we've
    // recorded in `AbsDecl::Impl`.
    let mut disappearing_decl_count = 0usize;
    for decl in decls.into_iter() {
        tcs = tcs.check_decl(decl, &mut disappearing_decl_count)?;
    }
    Ok(tcs)
}

fn require_local_emptiness(tcs: &TCS) {
    debug_assert!(tcs.local_env.is_empty());
    debug_assert!(tcs.local_gamma.is_empty());
}

fn check_decl(tcs: TCS, decl: AbsDecl, disappearing_decl_count: &mut usize) -> TCM {
    match decl {
        AbsDecl::Impl(impl_abs, sign_dbi) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let actual_index = sign_dbi - *disappearing_decl_count;
            let sign = tcs.glob_type(actual_index);
            let sign_cloned = sign.ast.clone();
            let (val_fake, mut tcs) = tcs.check(&impl_abs, &sign_cloned)?;
            let val = val_fake.map_ast(|ast| ast.axiom_to_var());

            tcs.env[actual_index] = val;
            *disappearing_decl_count += 1;
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
    pub fn check_decl(self, decl: AbsDecl, disappearing_decl_count: &mut usize) -> TCM {
        check_decl(self, decl, disappearing_decl_count)
    }
}
