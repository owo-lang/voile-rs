use voile_util::level::LiftEx;
use voile_util::uid::DBI;

use super::{CaseSplit, Closure, Neutral, Val, Variants};

/// Reducible expressions.
pub trait RedEx<T: Sized = Val>: Sized {
    /// This is primarily a private implementation-related API.
    /// Use at your own risk.
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> T;

    /// When the argument is not likely to be used,
    /// prefer this over [`reduce_with_dbi`](reduce_with_dbi).
    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> T;
}

impl RedEx for Val {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        match self {
            Val::Pair(a, b) => Val::pair(
                a.reduce_with_dbi_borrow(&arg, dbi),
                b.reduce_with_dbi(arg, dbi),
            ),
            Val::Neut(neutral_value) => neutral_value.reduce_with_dbi(arg, dbi),
            Val::Lam(closure) => Val::Lam(closure.reduce_with_dbi(arg, dbi + 1)),
            Val::Dt(kind, param_plicit, param_type, closure) => Val::dependent_type(
                kind,
                param_plicit,
                param_type.reduce_with_dbi_borrow(&arg, dbi),
                closure.reduce_with_dbi(arg, dbi + 1),
            ),
            Val::RowPoly(kind, variants) => {
                Val::RowPoly(kind, reduce_variants_with_dbi(variants, dbi, &arg))
            }
            Val::Rec(fields) => Val::Rec(reduce_variants_with_dbi(fields, dbi, &arg)),
            Val::Cons(name, a) => Self::cons(name, a.reduce_with_dbi(arg, dbi)),
            Val::Type(n) => Val::Type(n),
            Val::RowKind(l, k, ls) => Val::RowKind(l, k, ls),
        }
    }

    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Val {
        match self {
            Val::Pair(a, b) => Val::pair(
                a.reduce_with_dbi_borrow(arg, dbi),
                b.reduce_with_dbi_borrow(arg, dbi),
            ),
            Val::Neut(neutral_value) => neutral_value.reduce_with_dbi_borrow(arg, dbi),
            Val::Lam(closure) => Val::Lam(closure.reduce_with_dbi_borrow(arg, dbi + 1)),
            Val::Dt(kind, param_plicit, param_type, closure) => Val::dependent_type(
                kind,
                param_plicit,
                param_type.reduce_with_dbi_borrow(arg, dbi),
                closure.reduce_with_dbi_borrow(arg, dbi + 1),
            ),
            Val::RowPoly(kind, variants) => {
                Val::RowPoly(kind, reduce_variants_with_dbi(variants, dbi, arg))
            }
            Val::Rec(fields) => Val::Rec(reduce_variants_with_dbi(fields, dbi, arg)),
            Val::Cons(name, a) => Self::cons(name, a.reduce_with_dbi_borrow(arg, dbi)),
            Val::Type(n) => Val::Type(n),
            Val::RowKind(l, k, ls) => Val::RowKind(l, k, ls),
        }
    }
}

impl RedEx for Neutral {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        use Neutral::*;
        match self {
            Var(n) if dbi == n => arg.attach_dbi(dbi),
            Var(n) => Val::var(n),
            Ref(n) => Val::glob(n),
            Meta(mi) => Val::meta(mi),
            Axi(a) => Val::Neut(Axi(a)),
            App(f, args) => args
                .into_iter()
                .fold(f.reduce_with_dbi_borrow(&arg, dbi), |f, a| {
                    // Do we need to `reduce` after `apply` again?
                    f.apply(a.reduce_with_dbi_borrow(&arg, dbi))
                }),
            SplitOn(split, obj) => Val::case_tree(split)
                .apply(obj.reduce_with_dbi_borrow(&arg, dbi))
                // further reduce because the `split` is not yet reduced
                .reduce_with_dbi(arg, dbi),
            OrSplit(split, or) => Val::case_tree(split)
                .split_extend(or.reduce_with_dbi_borrow(&arg, dbi))
                .reduce_with_dbi(arg, dbi),
            Fst(pair) => pair.reduce_with_dbi(arg, dbi).first(),
            Snd(pair) => pair.reduce_with_dbi(arg, dbi).second(),
            Proj(rec, field) => rec.reduce_with_dbi(arg, dbi).project(field),
            Lift(levels, neut) => neut.reduce_with_dbi(arg, dbi).lift(levels),
            Row(kind, variants, ext) => {
                let variants = reduce_variants_with_dbi(variants, dbi, &arg);
                let ext = ext.reduce_with_dbi(arg, dbi);
                Val::RowPoly(kind, variants).row_extend(ext)
            }
            Rec(fields, ext) => {
                let fields = reduce_variants_with_dbi(fields, dbi, &arg);
                let ext = ext.reduce_with_dbi(arg, dbi);
                Val::Rec(fields).rec_extend(ext)
            }
        }
    }

    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Val {
        use Neutral::*;
        match self {
            Var(n) if dbi == n => arg.clone().attach_dbi(dbi),
            Var(n) => Val::var(n),
            Ref(n) => Val::glob(n),
            Meta(mi) => Val::meta(mi),
            Axi(a) => Val::Neut(Axi(a)),
            App(f, args) => args
                .into_iter()
                .fold(f.reduce_with_dbi_borrow(arg, dbi), |f, a| {
                    // Do we need to `reduce` after `apply` again?
                    f.apply(a.reduce_with_dbi_borrow(arg, dbi))
                }),
            SplitOn(split, obj) => Val::case_tree(split)
                .apply(obj.reduce_with_dbi_borrow(&arg, dbi))
                // further reduce because the `split` is not yet reduced
                .reduce_with_dbi_borrow(arg, dbi),
            OrSplit(split, or) => Val::case_tree(split)
                .split_extend(or.reduce_with_dbi_borrow(arg, dbi))
                .reduce_with_dbi_borrow(arg, dbi),
            Fst(pair) => pair.reduce_with_dbi_borrow(arg, dbi).first(),
            Snd(pair) => pair.reduce_with_dbi_borrow(arg, dbi).second(),
            Proj(pair, field) => pair.reduce_with_dbi_borrow(arg, dbi).project(field),
            Lift(levels, neut) => neut.reduce_with_dbi_borrow(arg, dbi).lift(levels),
            Row(kind, variants, ext) => {
                let variants = reduce_variants_with_dbi(variants, dbi, arg);
                let ext = ext.reduce_with_dbi_borrow(&arg, dbi);
                Val::RowPoly(kind, variants).row_extend(ext)
            }
            Rec(fields, ext) => {
                let fields = reduce_variants_with_dbi(fields, dbi, arg);
                let ext = ext.reduce_with_dbi_borrow(&arg, dbi);
                Val::Rec(fields).row_extend(ext)
            }
        }
    }
}

impl RedEx<Closure> for Closure {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Self {
        use Closure::*;
        match self {
            Plain(body) => Self::plain(body.reduce_with_dbi(arg, dbi)),
            Tree(split) => Tree(reduce_case_tree_with_dbi(split, dbi, &arg)),
        }
    }

    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Self {
        use Closure::*;
        match self {
            Plain(body) => Self::plain(body.reduce_with_dbi_borrow(arg, dbi)),
            Tree(split) => Tree(reduce_case_tree_with_dbi(split, dbi, arg)),
        }
    }
}

fn reduce_variants_with_dbi(variants: Variants, dbi: DBI, arg: &Val) -> Variants {
    variants
        .into_iter()
        .map(|(name, ty)| (name, ty.reduce_with_dbi_borrow(&arg, dbi)))
        .collect()
}

fn reduce_case_tree_with_dbi(cases: CaseSplit, dbi: DBI, arg: &Val) -> CaseSplit {
    cases
        .into_iter()
        .map(|(name, ty)| (name, ty.reduce_with_dbi_borrow(&arg, dbi)))
        .collect()
}
