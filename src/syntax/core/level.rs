use super::{Closure, Neutral, Val};
use voile_util::level::{
    calc_slice_plus_one_level, calc_tree_map_level, calc_tree_map_plus_one_level, lift_tree_map,
    Level, LevelCalcState, LevelType, LiftEx,
};

pub const TYPE_OMEGA: Val = Val::Type(Level::Omega);

impl LiftEx for Val {
    fn lift(self, levels: LevelType) -> Val {
        match self {
            Val::Type(l) => Val::Type(l + levels),
            Val::RowKind(l, k, ls) => Val::RowKind(l + levels, k, ls),
            Val::Lam(closure) => Val::Lam(closure.lift(levels)),
            Val::Dt(kind, plicit, param_type, closure) => {
                Val::dependent_type(kind, plicit, param_type.lift(levels), closure.lift(levels))
            }
            Val::RowPoly(kind, variants) => Val::RowPoly(kind, lift_tree_map(levels, variants)),
            Val::Rec(fields) => Val::Rec(lift_tree_map(levels, fields)),
            Val::Cons(name, e) => Val::cons(name, e.lift(levels)),
            Val::Pair(l, r) => Val::pair(l.lift(levels), r.lift(levels)),
            Val::Neut(neut) => Val::Neut(neut.lift(levels)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        match self {
            Val::Type(level) | Val::RowKind(level, ..) => Some(*level + 1),
            Val::RowPoly(_, variants) => calc_tree_map_level(variants),
            Val::Rec(fields) => calc_tree_map_level(fields),
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
    fn lift(self, levels: LevelType) -> Self {
        use super::Neutral::*;
        match self {
            Lift(n, expr) => Lift(n + levels, expr),
            Var(n) => Var(n),
            Ref(n) => Ref(n),
            Meta(n) => Meta(n),
            Axi(x) => Axi(x),
            App(f, args) => App(
                Box::new(f.lift(levels)),
                args.into_iter().map(|a| a.lift(levels)).collect(),
            ),
            Fst(p) => Fst(Box::new(p.lift(levels))),
            Snd(p) => Snd(Box::new(p.lift(levels))),
            Proj(r, n) => Proj(Box::new(r.lift(levels)), n),
            Row(kind, v, e) => Row(kind, lift_tree_map(levels, v), Box::new(e.lift(levels))),
            Rec(v, e) => Rec(lift_tree_map(levels, v), Box::new(e.lift(levels))),
            SplitOn(split, on) => SplitOn(lift_tree_map(levels, split), Box::new(on.lift(levels))),
            OrSplit(split, or) => OrSplit(lift_tree_map(levels, split), Box::new(or.lift(levels))),
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
            App(f, args) => calc_slice_plus_one_level(&**f, args),
            Rec(vs, ext) | Row(_, vs, ext) => calc_tree_map_plus_one_level(&**ext, vs),
            SplitOn(split, on) | OrSplit(split, on) => calc_tree_map_plus_one_level(&**on, split),
        }
    }
}

impl LiftEx for Closure {
    fn lift(self, levels: LevelType) -> Self {
        use super::Closure::*;
        match self {
            Plain(body) => Self::plain(body.lift(levels)),
            Tree(split) => Tree(lift_tree_map(levels, split)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        use super::Closure::*;
        match self {
            Plain(body) => body.calc_level(),
            Tree(split) => calc_tree_map_level(&split),
        }
    }
}
