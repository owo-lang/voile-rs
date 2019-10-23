use super::{Closure, Neutral, Val};
use std::cmp::Ordering;
use voile_util::level::{
    calc_slice_plus_one_level, calc_tree_map_level, calc_tree_map_plus_one_level, fall_tree_map,
    lift_tree_map, Level, LevelCalcState, LevelType, LiftEx,
};

pub const TYPE_OMEGA: Val = Val::Type(Level::Omega);

macro_rules! define_val_lift {
    ($lift:ident, $lift_tree:ident, $op:expr) => {
        fn $lift(self, levels: LevelType) -> Val {
            match self {
                Val::Type(l) => Val::Type($op(l, levels)),
                Val::RowKind(l, k, ls) => Val::RowKind($op(l, levels), k, ls),
                Val::Lam(closure) => Val::Lam(closure.$lift(levels)),
                Val::Dt(kind, plicit, param_type, closure) => {
                    Val::dependent_type(kind, plicit, param_type.$lift(levels), closure.$lift(levels))
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
    define_val_lift!(lift, lift_tree_map, ::std::ops::Add::add);
    define_val_lift!(fall, fall_tree_map, ::std::ops::Sub::sub);

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

// I wish this can be type-directed :(
macro_rules! define_neut_lift {
    ($lift:ident, $lift_tree:ident, $ref_op:ident, $lift_op:expr, $fall_op:expr) => {
        fn $lift(self, levels: LevelType) -> Self {
            use super::Neutral::*;
            match self {
                Lift(n, expr) => $lift_op(n, expr, levels),
                Fall(n, expr) => $fall_op(n, expr, levels),
                Var(n) => Var(n),
                Ref(n) => $ref_op(levels, Box::new(Ref(n))),
                Meta(n) => Meta(n),
                Axi(x) => Axi(x),
                App(f, args) => App(
                    Box::new(f.$lift(levels)),
                    args.into_iter().map(|a| a.$lift(levels)).collect(),
                ),
                Fst(p) => Fst(Box::new(p.$lift(levels))),
                Snd(p) => Snd(Box::new(p.$lift(levels))),
                Proj(r, n) => Proj(Box::new(r.$lift(levels)), n),
                Row(kind, v, e) => Row(kind, $lift_tree(levels, v), Box::new(e.$lift(levels))),
                Rec(v, e) => Rec($lift_tree(levels, v), Box::new(e.$lift(levels))),
                SplitOn(split, on) => SplitOn($lift_tree(levels, split), Box::new(on.$lift(levels))),
                OrSplit(split, or) => OrSplit($lift_tree(levels, split), Box::new(or.$lift(levels))),
            }
        }
    }
}

impl LiftEx for Neutral {
    define_neut_lift!(
        lift,
        lift_tree_map,
        Lift,
        |n: LevelType, expr: Box<Neutral>, levels: LevelType| Lift(n + levels, expr),
        |n: LevelType, expr: Box<Neutral>, levels: LevelType| match n.cmp(&levels) {
            Ordering::Less => Lift(levels - n, expr),
            Ordering::Equal => *expr,
            Ordering::Greater => Fall(n - levels, expr),
        }
    );
    define_neut_lift!(
        fall,
        fall_tree_map,
        Fall,
        |n: LevelType, expr: Box<Neutral>, levels: LevelType| match n.cmp(&levels) {
            Ordering::Less => Fall(levels - n, expr),
            Ordering::Equal => *expr,
            Ordering::Greater => Lift(n - levels, expr),
        },
        |n: LevelType, expr: Box<Neutral>, levels: LevelType| Lift(n + levels, expr)
    );

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
        fn $lift(self, levels: LevelType) -> Self {
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
