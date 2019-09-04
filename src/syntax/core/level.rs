use super::{Closure, Neutral, Val};
use std::collections::BTreeMap;
use voile_util::level::{Level, LevelCalcState, LiftEx};

pub const TYPE_OMEGA: Val = Val::Type(Level::Omega);

impl LiftEx for Val {
    fn lift(self, levels: u32) -> Val {
        match self {
            Val::Type(l) => Val::Type(l + levels),
            Val::RowKind(l, k, ls) => Val::RowKind(l + levels, k, ls),
            Val::Lam(closure) => Val::Lam(closure.lift(levels)),
            Val::Dt(kind, plicit, param_type, closure) => Val::Dt(
                kind,
                plicit,
                Box::new(param_type.lift(levels)),
                closure.lift(levels),
            ),
            Val::RowPoly(kind, variants) => Val::RowPoly(kind, lift_map(levels, variants)),
            Val::Rec(fields) => Val::Rec(lift_map(levels, fields)),
            Val::Cons(name, e) => Val::cons(name, e.lift(levels)),
            Val::Pair(l, r) => Val::pair(l.lift(levels), r.lift(levels)),
            Val::Neut(neut) => Val::Neut(neut.lift(levels)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        match self {
            Val::Type(level) | Val::RowKind(level, ..) => Some(*level + 1),
            Val::RowPoly(_, variants) => calc_map_level(variants),
            Val::Rec(fields) => calc_map_level(fields),
            Val::Dt(_, _, param_ty, closure) => {
                Some(param_ty.calc_level()?.max(closure.calc_level()?))
            }
            Val::Lam(closure) => closure.calc_level(),
            Val::Neut(neut) => neut.calc_level(),
            Val::Pair(l, r) => Some(l.calc_level()?.max(r.calc_level()?)),
            Val::Cons(_, e) => e.calc_level(),
        }
    }
}

impl LiftEx for Neutral {
    fn lift(self, levels: u32) -> Self {
        use super::Neutral::*;
        match self {
            Lift(n, expr) => Lift(n + levels, expr),
            e => Lift(levels, Box::new(e)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        use super::Neutral::*;
        match self {
            Lift(n, expr) => match expr.calc_level() {
                Some(m) => Some(m + *n),
                // Trying to lift yourself makes you omega.
                None => Some(Level::Omega),
            },
            // Level is zero by default
            Var(..) | Axi(..) | Meta(..) => Some(Default::default()),
            Ref(..) => None,
            Fst(expr) => expr.calc_level(),
            Snd(expr) => expr.calc_level(),
            Proj(expr, ..) => expr.calc_level(),
            App(f, args) => {
                let args: Option<Vec<_>> = args.iter().map(LiftEx::calc_level).collect();
                let args = args?.into_iter().max();
                Some(f.calc_level()?.max(args.unwrap_or_default()))
            }
            Rec(vs, ext) | Row(_, vs, ext) => {
                let vs = calc_map_level(vs);
                Some(ext.calc_level()?.max(vs.unwrap_or_default()))
            }
            SplitOn(split, on) | OrSplit(split, on) => {
                let split = calc_map_level(split);
                Some(on.calc_level()?.max(split.unwrap_or_default()))
            }
        }
    }
}

impl LiftEx for Closure {
    fn lift(self, levels: u32) -> Self {
        use super::Closure::*;
        match self {
            Plain(body) => Self::plain(body.lift(levels)),
            Tree(split) => Tree(lift_map(levels, split)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        use super::Closure::*;
        match self {
            Plain(body) => body.calc_level(),
            Tree(split) => calc_map_level(&split),
        }
    }
}

fn lift_map<T: LiftEx>(levels: u32, fields: BTreeMap<String, T>) -> BTreeMap<String, T> {
    fields
        .into_iter()
        .map(|(name, e)| (name, e.lift(levels)))
        .collect()
}

fn calc_map_level(variants: &BTreeMap<String, impl LiftEx>) -> LevelCalcState {
    let levels: Option<Vec<_>> = variants.values().map(LiftEx::calc_level).collect();
    Some(levels?.into_iter().max().unwrap_or_default())
}
