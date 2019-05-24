use std::collections::btree_map::BTreeMap;

use crate::check::monad::TCS;
use crate::syntax::abs::Abs;
use crate::syntax::common::{SyntaxInfo, ToSyntaxInfo};
use crate::syntax::core::{Closure, Term, TermInfo};

/// Term-producing strategy -- whether to produce
/// dbi-based variables or uid-based variables.
#[derive(Debug, Clone, Copy)]
enum Strategy {
    Check,
    Evaluate,
}

/// This function will produce **dbi**-based variable references.
///
/// Ensure `abs` is well-typed before invoking this,
/// otherwise this function may panic or produce ill-typed core term.
fn unsafe_compile(tcs: TCS, strategy: Strategy, abs: Abs) -> (TermInfo, TCS) {
    match abs {
        Abs::Type(info, level) => (Term::Type(level).into_info(info), tcs),
        Abs::Bot(info) => (Term::Bot(0).into_info(info), tcs),
        Abs::Local(info, _, dbi) => match strategy {
            Strategy::Check => (tcs.local_val(dbi).ast.into_info(info), tcs),
            Strategy::Evaluate => (Term::var(dbi).into_info(info), tcs),
        },
        Abs::Var(info, dbi) => (tcs.glob_val(dbi).ast.into_info(info), tcs),
        Abs::App(info, f, a) => {
            let (f, tcs) = unsafe_compile(tcs, Strategy::Check, *f);
            let (a, tcs) = unsafe_compile(tcs, strategy, *a);
            (f.ast.apply(a.ast).into_info(info), tcs)
        }
        Abs::Dt(info, kind, _, param_ty, ret_ty) => {
            let (param_ty, tcs) = unsafe_compile(tcs, strategy, *param_ty);
            let (ret_ty, tcs) = unsafe_compile(tcs, strategy, *ret_ty);
            let closure = Closure::new(param_ty.ast, ret_ty.ast);
            let term = Term::Dt(kind, closure);
            (term.into_info(info), tcs)
        }
        Abs::Pair(info, a, b) => {
            let (a, tcs) = unsafe_compile(tcs, strategy, *a);
            let (b, tcs) = unsafe_compile(tcs, strategy, *b);
            (Term::pair(a.ast, b.ast).into_info(info), tcs)
        }
        Abs::Fst(info, p) => {
            let (p, tcs) = unsafe_compile(tcs, strategy, *p);
            (p.ast.first().into_info(info), tcs)
        }
        Abs::Snd(info, p) => {
            let (p, tcs) = unsafe_compile(tcs, strategy, *p);
            (p.ast.second().into_info(info), tcs)
        }
        Abs::Lam(info, _, _, body) => {
            let (body, tcs) = unsafe_compile(tcs, strategy, *body);
            (body.ast.into_info(info), tcs)
        }
        e => panic!("Cannot compile `{}` at {}", e, e.syntax_info()),
    }
}

pub fn compile_cons_type(info: &SyntaxInfo, ret_ty: &Closure) -> TermInfo {
    let mut variant = BTreeMap::default();
    variant.insert(info.text.clone(), Term::var(0));
    let lam = Term::lam(*ret_ty.param_type.clone(), Term::Sum(variant));
    lam.into_info(info.clone())
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn unsafe_compile(self, abs: Abs) -> (TermInfo, Self) {
        unsafe_compile(self, Strategy::Check, abs)
    }

    #[inline]
    pub fn unsafe_evaluate(self, abs: Abs) -> (TermInfo, Self) {
        unsafe_compile(self, Strategy::Evaluate, abs)
    }
}
