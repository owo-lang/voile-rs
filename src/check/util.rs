use std::collections::btree_map::BTreeMap;

use crate::check::monad::TCS;
use crate::syntax::abs::Abs;
use crate::syntax::common::{SyntaxInfo, ToSyntaxInfo};
use crate::syntax::core::{Closure, Val, ValInfo};

/// Term-producing strategy -- whether to produce
/// dbi-based variables or uid-based variables.
#[derive(Debug, Clone, Copy)]
enum Strategy {
    /// During type-checking, produce uid-based terms (which can be further type-checked).
    Check,
    /// During compilation, produce dbi-based terms (which can be further evaluated).
    Evaluate,
}

/// Given the original `Abs` and the type-checked `Term` (if available).
/// In order to have that term, you'll need to type-check the `abs` first,
/// which guarantees `abs` to be well-typed.
/// This function will be unsafe if `checked` is `None`.
fn compile(tcs: TCS, strategy: Strategy, abs: Abs, checked: Option<Val>) -> (ValInfo, TCS) {
    match abs {
        Abs::Type(info, level) => (Val::Type(level).into_info(info), tcs),
        Abs::Bot(info) => (Val::Bot(0).into_info(info), tcs),
        Abs::Local(info, _, dbi) => match strategy {
            Strategy::Check => (tcs.local_val(dbi).ast.into_info(info), tcs),
            Strategy::Evaluate => (Val::var(dbi).into_info(info), tcs),
        },
        Abs::Var(info, dbi) => (tcs.glob_val(dbi).ast.into_info(info), tcs),
        // Because I don't know what else can I output.
        Abs::Variant(info) => (compile_cons_type(info, Val::axiom()), tcs),
        Abs::App(info, f, a) => {
            // The function should always be compiled to DBI-based terms
            let (f, tcs) = compile(tcs, Strategy::Evaluate, *f, None);
            let (a, tcs) = compile(tcs, strategy, *a, None);
            (f.ast.apply(a.ast).into_info(info), tcs)
        }
        Abs::Dt(info, kind, _, param_ty, ret_ty) => match checked {
            Some(Val::Dt(kind, closure)) => {
                let (param_ty, tcs) = compile(tcs, strategy, *param_ty, Some(*closure.param_type));
                let (ret_ty, tcs) = compile(tcs, strategy, *ret_ty, Some(*closure.body));
                let closure = Closure::new(param_ty.ast, ret_ty.ast);
                let term = Val::Dt(kind, closure);
                (term.into_info(info), tcs)
            }
            _ => {
                let (param_ty, tcs) = compile(tcs, strategy, *param_ty, None);
                let (ret_ty, tcs) = compile(tcs, strategy, *ret_ty, None);
                let closure = Closure::new(param_ty.ast, ret_ty.ast);
                let term = Val::Dt(kind, closure);
                (term.into_info(info), tcs)
            }
        },
        Abs::Pair(info, a, b) => match checked {
            Some(Val::Pair(checked_a, checked_b)) => {
                let (a, tcs) = compile(tcs, strategy, *a, Some(*checked_a));
                let (b, tcs) = compile(tcs, strategy, *b, Some(*checked_b));
                (Val::pair(a.ast, b.ast).into_info(info), tcs)
            }
            _ => {
                let (a, tcs) = compile(tcs, strategy, *a, None);
                let (b, tcs) = compile(tcs, strategy, *b, None);
                (Val::pair(a.ast, b.ast).into_info(info), tcs)
            }
        },
        Abs::Fst(info, p) => {
            let (p, tcs) = compile(tcs, strategy, *p, None);
            (p.ast.first().into_info(info), tcs)
        }
        Abs::Snd(info, p) => {
            let (p, tcs) = compile(tcs, strategy, *p, None);
            (p.ast.second().into_info(info), tcs)
        }
        Abs::Lam(info, _, _, body) => match checked {
            Some(Val::Lam(closure)) => {
                let (body, tcs) = compile(tcs, strategy, *body, Some(*closure.body));
                let lam = Val::lam(*closure.param_type, body.ast).into_info(info);
                (lam, tcs)
            }
            _ => {
                let (body, tcs) = compile(tcs, strategy, *body, None);
                (body.ast.into_info(info), tcs)
            }
        },
        e => panic!("Cannot compile `{}` at {}", e, e.syntax_info()),
    }
}

pub fn compile_cons_type(info: SyntaxInfo, ret_ty: Val) -> ValInfo {
    let mut variant = BTreeMap::default();
    variant.insert(info.text.clone(), Val::var(0));
    let lam = Val::lam(ret_ty, Val::Sum(variant));
    lam.into_info(info)
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn evaluate(self, abs: Abs) -> (ValInfo, Self) {
        compile(self, Strategy::Check, abs, None)
    }

    #[inline]
    pub fn compile(self, abs: Abs, checked: Option<Val>) -> (ValInfo, Self) {
        compile(self, Strategy::Evaluate, abs, checked)
    }
}
