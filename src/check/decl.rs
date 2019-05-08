use crate::syntax::abs::AbsDecl;

use super::expr::{check, check_type};
use super::monad::{TCM, TCS};

pub fn check_decls(mut tcs: TCS, decls: Vec<AbsDecl>) -> TCM {
    for decl in decls.into_iter() {
        tcs = check_decl(tcs, decl.clone())?;
    }
    Ok(tcs)
}

pub fn check_decl(tcs: TCS, decl: AbsDecl) -> TCM {
    match decl {
        AbsDecl::Both(sign_info, sign_abs, _impl_info, impl_abs) => {
            debug_assert_eq!(tcs.gamma.len(), tcs.env.len());
            let (val, mut tcs) = check_type(tcs, sign_abs)?;
            tcs.gamma.push(val.ast.clone().into_info(sign_info));

            let (val, mut tcs) = check(tcs, impl_abs, val.ast)?;
            tcs.env.push(val);
            // Err(TCE::DbiOverflow(tcs.env.len(), new_dbi))
            Ok(tcs)
        }
        _ => unreachable!(),
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
