use super::{Closure, Neutral, Val};
use std::cmp::Ordering;
use voile_util::level::{
    calc_slice_plus_one_level, calc_tree_map_level, calc_tree_map_plus_one_level, fall_tree_map,
    lift_tree_map, Level, LevelCalcState, LiftEx,
};

pub const TYPE_OMEGA: Val = Val::Type(Level::Omega);

macro_rules! define_val_lift {
    ($lift:ident, $fall:ident, $lift_tree:ident, $op:expr) => {
        fn $lift(self, levels: u32) -> Val {
            match self {
                Val::Type(l) => Val::Type($op(l, levels)),
                Val::RowKind(l, k, ls) => Val::RowKind($op(l, levels), k, ls),
                Val::Lam(closure) => Val::Lam(closure.$lift(levels)),
                Val::Dt(kind, plicit, param_type, closure) => {
                    Val::dependent_type(kind, plicit, param_type.$fall(levels), closure.$lift(levels))
                }
                Val::RowPoly(kind, variants) => Val::RowPoly(kind, $lift_tree(levels, variants)),
                Val::Rec(fields) => Val::Rec($lift_tree(levels, fields)),
                Val::Cons(name, e) => Val::cons(name, e.$lift(levels)),
                Val::Pair(l, r) => Val::pair(l.$lift(levels), r.$lift(levels)),
                Val::Neut(neut) => Val::Neut(neut.$lift(levels)),
            }
        }
    };
}

impl LiftEx for Val {
    define_val_lift!(lift, fall, lift_tree_map, ::std::ops::Add::add);
    define_val_lift!(fall, lift, fall_tree_map, ::std::ops::Sub::sub);

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
    fn lift(self, levels: u32) -> Self {
        use super::Neutral::*;
        match self {
            Lift(n, expr) => Lift(n + levels, expr),
            Fall(n, expr) => match n.cmp(&levels) {
                Ordering::Less => Lift(levels - n, expr),
                Ordering::Equal => *expr,
                Ordering::Greater => Fall(n - levels, expr),
            },
            e => Lift(levels, Box::new(e)),
        }
    }

    fn fall(self, levels: u32) -> Self {
        use super::Neutral::*;
        match self {
            Fall(n, expr) => Lift(n + levels, expr),
            Lift(n, expr) => match n.cmp(&levels) {
                Ordering::Less => Fall(levels - n, expr),
                Ordering::Equal => *expr,
                Ordering::Greater => Lift(n - levels, expr),
            },
            e => Fall(levels, Box::new(e)),
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
            Fall(n, expr) => match expr.calc_level() {
                Some(m) => Some(m - *n),
                None => unreachable!(),
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

macro_rules! define_clos_lift {
    ($lift:ident, $lift_tree:ident) => {
        fn $lift(self, levels: u32) -> Self {
            use super::Closure::*;
            match self {
                Plain(body) => Self::plain(body.$lift(levels)),
                Tree(split) => Tree($lift_tree(levels, split)),
            }
        }
    }
}

impl LiftEx for Closure {
    define_clos_lift!(lift, lift_tree_map);
    define_clos_lift!(fall, fall_tree_map);

    fn calc_level(&self) -> LevelCalcState {
        use super::Closure::*;
        match self {
            Plain(body) => body.calc_level(),
            Tree(split) => calc_tree_map_level(&split),
        }
    }
}
