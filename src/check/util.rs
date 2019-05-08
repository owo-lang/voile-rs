use crate::check::monad::TCS;
use crate::syntax::abs::Abs;
use crate::syntax::common::ParamKind;
use crate::syntax::core::{Closure, Term, TermInfo};

/// Ensure `abs` is well-typed before invoking this,
/// otherwise this function may panic or produce ill-typed core term.
pub fn unsafe_compile(tcs: TCS, abs: Abs) -> (TermInfo, TCS) {
    match abs {
        Abs::Type(info, level) => (Term::Type(level).into_info(info), tcs),
        Abs::Bot(info) => (Term::Bot(0).into_info(info), tcs),
        // TODO: where can I access global env? Is it `tcs.env`?
        Abs::Var(info, dbi) => unimplemented!(),
        Abs::Local(info, dbi) => (Term::var(dbi).into_info(info), tcs),
        Abs::Meta(info) => panic!("Cannot compile meta variable at {}.", info),
        Abs::Cons(info) => unimplemented!(),
        Abs::ConsType(_) => unimplemented!(),
        Abs::App(info, f, a) => {
            let (f, tcs) = unsafe_compile(tcs, *f);
            let (a, tcs) = unsafe_compile(tcs, *a);
            (f.ast.apply(a.ast).into_info(info), tcs)
        }
        Abs::Dt(info, kind, param_ty, ret_ty) => {
            let (param_ty, tcs) = unsafe_compile(tcs, *param_ty);
            let (ret_ty, tcs) = unsafe_compile(tcs, *ret_ty);
            // TODO: implicit arguments
            let closure = Closure::new(param_ty.ast, ret_ty.ast);
            let term = Term::dependent_type(ParamKind::Explicit, kind, closure);
            (term.into_info(info), tcs)
        }
        Abs::Pair(info, a, b) => {
            let (a, tcs) = unsafe_compile(tcs, *a);
            let (b, tcs) = unsafe_compile(tcs, *b);
            (Term::pair(a.ast, b.ast).into_info(info), tcs)
        }
        Abs::Fst(info, p) => {
            let (p, tcs) = unsafe_compile(tcs, *p);
            (p.ast.first().into_info(info), tcs)
        }
        Abs::Snd(info, p) => {
            let (p, tcs) = unsafe_compile(tcs, *p);
            (p.ast.second().into_info(info), tcs)
        }
        Abs::Sum(_, _) => unimplemented!(),
    }
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    pub fn unsafe_compile(self, abs: Abs) -> (TermInfo, Self) {
        unsafe_compile(self, abs)
    }
}
