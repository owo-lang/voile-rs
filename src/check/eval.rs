use std::collections::btree_map::BTreeMap;

use crate::check::monad::TCS;
use crate::syntax::abs::Abs;
use crate::syntax::common::{Ident, ToSyntaxInfo};
use crate::syntax::core::{LiftEx, Val, ValInfo};

/// Ensure `abs` is well-typed before invoking this,
/// otherwise this function may panic or produce ill-typed core term.
fn evaluate(mut tcs: TCS, abs: Abs) -> (ValInfo, TCS) {
    match abs {
        Abs::Type(info, level) => (Val::Type(level).into_info(info), tcs),
        Abs::Bot(info) => (Val::bot().into_info(info), tcs),
        Abs::Local(info, _, i) => (tcs.local_val(i).ast.attach_dbi(i).into_info(info.info), tcs),
        Abs::Var(info, dbi) => (tcs.glob_val(dbi).ast.into_info(info.info), tcs),
        // Because I don't know what else can I output.
        Abs::Variant(info) => (compile_variant(info, Val::axiom()), tcs),
        Abs::Cons(info) => (compile_cons(info, Val::axiom()), tcs),
        Abs::App(info, f, a) => {
            // The function should always be compiled to DBI-based terms
            let (f, tcs) = evaluate(tcs, *f);
            let (a, tcs) = evaluate(tcs, *a);
            let applied = f.ast.apply(a.ast);
            (applied.into_info(info), tcs)
        }
        Abs::Dt(info, kind, _, param_ty, ret_ty) => {
            let (param_ty, tcs) = evaluate(tcs, *param_ty);
            let (ret_ty, tcs) = evaluate(tcs, *ret_ty);
            let term = Val::dependent_type(kind, param_ty.ast, ret_ty.ast);
            (term.into_info(info), tcs)
        }
        Abs::Sum(info, sums) => {
            let mut variants = BTreeMap::default();
            for sum in sums {
                let (val, new_tcs) = evaluate(tcs, sum);
                tcs = new_tcs;
                if let Val::Sum(mut new) = val.ast {
                    variants.append(&mut new);
                } else {
                    panic!("Compile failed: not a sum, at {}.", val.info);
                }
            }
            (Val::Sum(variants).into_info(info), tcs)
        }
        Abs::Pair(info, a, b) => {
            let (a, tcs) = evaluate(tcs, *a);
            let (b, tcs) = evaluate(tcs, *b);
            (Val::pair(a.ast, b.ast).into_info(info), tcs)
        }
        Abs::Fst(info, p) => {
            let (p, tcs) = evaluate(tcs, *p);
            (p.ast.first().into_info(info), tcs)
        }
        Abs::Snd(info, p) => {
            let (p, tcs) = evaluate(tcs, *p);
            (p.ast.second().into_info(info), tcs)
        }
        // This branch is not likely to be reached.
        Abs::Lam(info, _, _, body) => {
            let (body, tcs) = evaluate(tcs, *body);
            (body.ast.into_info(info), tcs)
        }
        Abs::Lift(info, levels, expr) => {
            let (expr, tcs) = evaluate(tcs, *expr);
            (expr.ast.lift(levels).into_info(info), tcs)
        }
        e => panic!("Cannot compile `{}` at {}", e, e.syntax_info()),
    }
}

pub fn compile_variant(info: Ident, ret_ty: Val) -> ValInfo {
    let mut variant = BTreeMap::default();
    let mut text = info.text;
    text.remove(0);
    variant.insert(text, Val::var(0));
    Val::lam(ret_ty, Val::Sum(variant)).into_info(info.info)
}

pub fn compile_cons(info: Ident, ret_ty: Val) -> ValInfo {
    let mut text = info.text;
    text.remove(0);
    Val::lam(ret_ty, Val::cons(text, Val::var(0))).into_info(info.info)
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    /// Should be invoked **only** during type-checking,
    /// produce uid-based terms (which can be further type-checked).
    #[inline]
    pub fn evaluate(self, abs: Abs) -> (ValInfo, Self) {
        evaluate(self, abs)
    }
}
