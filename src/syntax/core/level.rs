use crate::syntax::core::{Closure, Neutral, Val, Variants};
use crate::syntax::level::Level;

/// Internal API, public only because it's used in public traits' internal APIs.
/// Produced during level calculation.<br/>
/// `Some(Level)` -- level of non-recursive definitions.<br/>
/// `None` -- level of self-reference.<br/>
/// Trying to lift this will result in omega, otherwise it should be computed as 0 level.
type LevelCalcState = Option<Level>;

pub const TYPE_OMEGA: Val = Val::Type(Level::Omega);

/// Expression with universe level (which means they can be lifted).
pub trait LiftEx: Sized {
    /// Lift the level of `self`.
    fn lift(self, levels: u32) -> Self;

    /// Internal API, for code sharing only.
    fn calc_level(&self) -> LevelCalcState;

    /// Calculate the level of `self`,
    /// like a normal value will have level 0,
    /// a type expression will have level 1 (or higher).
    fn level(&self) -> Level {
        self.calc_level().unwrap_or_default()
    }
}

impl LiftEx for Val {
    fn lift(self, levels: u32) -> Val {
        match self {
            Val::Type(l) => Val::Type(l + levels),
            Val::RowKind(l, k, ls) => Val::RowKind(l + levels, k, ls),
            Val::Lam(closure) => Val::Lam(closure.lift(levels)),
            Val::Dt(kind, param_type, closure) => Val::Dt(
                kind,
                Box::new(param_type.lift(levels)),
                closure.lift(levels),
            ),
            Val::RowPoly(kind, variants) => Val::RowPoly(kind, lift_variants(levels, variants)),
            Val::Rec(fields) => Val::Rec(lift_variants(levels, fields)),
            Val::Cons(name, e) => Val::cons(name, e.lift(levels)),
            Val::Pair(l, r) => Val::pair(l.lift(levels), r.lift(levels)),
            Val::Neut(neut) => Val::Neut(neut.lift(levels)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        match self {
            Val::Type(level) | Val::RowKind(level, ..) => Some(*level + 1),
            Val::RowPoly(_, variants) => calc_variants_level(variants),
            Val::Rec(fields) => calc_variants_level(fields),
            Val::Dt(_, param_ty, closure) => {
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
        use self::Neutral::*;
        match self {
            Lift(n, expr) => Lift(n + levels, expr),
            e => Lift(levels, Box::new(e)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        use self::Neutral::*;
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
            App(f, args) => {
                let args: Option<Vec<_>> = args.iter().map(LiftEx::calc_level).collect();
                let args = args?.into_iter().max();
                Some(f.calc_level()?.max(args.unwrap_or_default()))
            }
            Rec(vs, ext) | Row(_, vs, ext) => {
                let vs = calc_variants_level(vs);
                Some(ext.calc_level()?.max(vs.unwrap_or_default()))
            }
        }
    }
}

impl LiftEx for Closure {
    fn lift(self, levels: u32) -> Self {
        use Closure::*;
        match self {
            Plain(body) => Self::plain(body.lift(levels)),
        }
    }

    fn calc_level(&self) -> LevelCalcState {
        use Closure::*;
        match self {
            Plain(body) => body.calc_level(),
        }
    }
}

fn lift_variants(levels: u32, fields: Variants) -> Variants {
    fields
        .into_iter()
        .map(|(name, e)| (name, e.lift(levels)))
        .collect()
}

fn calc_variants_level(variants: &Variants) -> Option<Level> {
    let levels: Option<Vec<_>> = variants.values().map(LiftEx::calc_level).collect();
    Some(levels?.into_iter().max().unwrap_or_default())
}
