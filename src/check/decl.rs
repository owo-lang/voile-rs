use std::mem::swap;

use crate::syntax::abs::AbsDecl;
use crate::syntax::common::ToSyntaxInfo;
use crate::syntax::core::{Neutral, Val, ValInfo};

use super::monad::{TCM, TCS};

pub fn check_decls(tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    decls.into_iter().try_fold(tcs, check_decl)
}

fn require_local_emptiness(tcs: &TCS) {
    debug_assert!(tcs.local_env.is_empty());
    debug_assert!(tcs.local_gamma.is_empty());
}

fn unimplemented_to_glob(v: &mut [ValInfo], i: usize) {
    let mut placeholder = Default::default();
    swap(&mut v[i], &mut placeholder);
    placeholder = placeholder.map_ast(|ast| ast.unimplemented_to_glob());
    swap(&mut placeholder, &mut v[i]);
}

fn inline_metas(mut tcs: TCS, val: ValInfo) -> (ValInfo, TCS) {
    use Neutral::*;
    let val = val.map_ast(|ast| {
        ast.map_neutral(&mut |neut| match neut {
            // TODO: Provide error message instead of panic
            Meta(mi) => tcs.take_meta(mi).expect("Unsolved metas!"),
            e => Val::Neut(e),
        })
    });
    (val, tcs)
}

fn check_decl(tcs: TCS, decl: AbsDecl) -> TCM {
    debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
    let tcs = match decl {
        AbsDecl::Impl(impl_abs, sign_dbi) => {
            let sign = tcs.glob_type(sign_dbi);
            let sign_cloned = sign.ast.clone();
            let (val_fake, tcs) = tcs.check(&impl_abs, &sign_cloned)?;
            // We generate axioms for lambda parameters during type-checking.
            // Now it's time to change them back to `var` references.
            let val = val_fake.map_ast(|ast| ast.generated_to_var());
            let (val, mut tcs) = inline_metas(tcs, val);

            tcs.env[sign_dbi.0] = val;

            // Every references to me are now actually valid (they were axioms before),
            // replace them with a global reference.
            for i in sign_dbi.0..tcs.glob_len() {
                unimplemented_to_glob(&mut tcs.env, i);
            }
            for i in sign_dbi.0 + 1..tcs.glob_len() {
                unimplemented_to_glob(&mut tcs.gamma, i);
            }

            // Err(TCE::DbiOverflow(tcs.env.len(), new_dbi))
            tcs
        }
        AbsDecl::Sign(sign_abs, self_index) => {
            let syntax_info = sign_abs.syntax_info();
            let (sign_fake, tcs) = tcs.check_type(&sign_abs)?;
            let (sign_fake, mut tcs) = inline_metas(tcs, sign_fake);
            let sign = sign_fake.map_ast(|ast| ast.generated_to_var());
            let val_info = Val::fresh_unimplemented(self_index).into_info(syntax_info);
            tcs.env.push(val_info);
            tcs.gamma.push(sign);

            // Give warning on axiom?
            tcs
        }
        AbsDecl::Decl(impl_abs) => {
            let (inferred, tcs) = tcs.infer(&impl_abs)?;
            let (inferred, tcs) = inline_metas(tcs, inferred);
            let (compiled, tcs) = tcs.evaluate(impl_abs);
            let (compiled, mut tcs) = inline_metas(tcs, compiled);
            let compiled = compiled.map_ast(|ast| ast.generated_to_var());
            let inferred = inferred.map_ast(|ast| ast.generated_to_var());
            tcs.env.push(compiled);
            tcs.gamma.push(inferred);

            tcs
        }
    };

    require_local_emptiness(&tcs);
    Ok(tcs)
}

impl TCS {
    #[inline]
    pub fn check_decls(self, decls: Vec<AbsDecl>) -> TCM {
        check_decls(self, decls)
    }
}
