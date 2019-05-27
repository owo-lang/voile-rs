use std::collections::btree_map::BTreeMap;

use crate::syntax::common::{next_uid, DtKind, Level, DBI, UID};

pub type NamedEnv = BTreeMap<String, Val>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// This is primarily a private implementation-related API.
    /// Use at your own risk.
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val;

    /// Instantiate `self` as a closure (possibly neutral terms) with
    /// a concrete argument.
    #[inline]
    fn instantiate(self, arg: Val) -> Val {
        self.reduce_with_dbi(arg, 0)
    }
}

impl Val {
    /// Just for evaluation during beta-reduction.
    pub fn apply(self, arg: Val) -> Val {
        match self {
            Val::Lam(closure) => closure.body.instantiate(arg),
            Val::Neut(n) => Val::app(n, arg),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    /// Just for evaluation during beta-reduction.
    pub fn first(self) -> Val {
        match self {
            Val::Pair(a, _) => *a,
            Val::Neut(n) => Val::fst(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    /// Just for evaluation during beta-reduction.
    pub fn second(self) -> Val {
        match self {
            Val::Pair(_, b) => *b,
            Val::Neut(n) => Val::snd(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }
}

impl RedEx for Val {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        match self {
            Val::Pair(a, b) => Val::pair(
                a.reduce_with_dbi(arg.clone(), dbi),
                b.reduce_with_dbi(arg, dbi),
            ),
            Val::Neut(neutral_value) => neutral_value.reduce_with_dbi(arg, dbi),
            Val::Lam(Closure { param_type, body }) => Val::lam(
                param_type.reduce_with_dbi(arg.clone(), dbi),
                body.reduce_with_dbi(arg, dbi + 1),
            ),
            Val::Dt(kind, Closure { param_type, body }) => Val::dependent_type(
                kind,
                param_type.reduce_with_dbi(arg.clone(), dbi),
                body.reduce_with_dbi(arg, dbi + 1),
            ),
            // Cannot reduce
            e => e,
        }
    }
}

/// Irreducible because of the presence of generated value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Neutral {
    /// Local variable, referred by de-bruijn index.
    Var(DBI),
    /// Postulated value, aka axioms.
    /// If the `Option<DBI>` value is `None`, then it's really postulated.
    /// Otherwise it should be a generated lambda parameter.
    Axi(UID, Option<DBI>),
    /// Function application.
    App(Box<Self>, Box<Val>),
    /// Projecting the first element of a pair.
    Fst(Box<Self>),
    /// Projecting the second element of a pair.
    Snd(Box<Self>),
}

impl Neutral {
    pub fn axiom_to_var(self) -> Self {
        self.map_axiom(|uid, dbi| dbi.map_or_else(|| Neutral::Axi(uid, None), Neutral::Var))
    }

    pub fn map_axiom<F: Fn(UID, Option<DBI>) -> Self + Copy>(self, f: F) -> Self {
        match self {
            Neutral::Axi(uid, dbi) => f(uid, dbi),
            Neutral::App(fun, a) => Neutral::App(
                Box::new(fun.map_axiom(f)),
                Box::new(a.map_neutral(|n| n.map_axiom(f))),
            ),
            Neutral::Fst(p) => Neutral::Fst(Box::new(p.map_axiom(f))),
            Neutral::Snd(p) => Neutral::Snd(Box::new(p.map_axiom(f))),
            e => e,
        }
    }
}

impl RedEx for Neutral {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        use self::Neutral::*;
        match self {
            Var(n) if dbi == n => arg,
            Var(n) => Val::var(n),
            Axi(i, dbi) => Val::Neut(Axi(i, dbi)),
            App(function, argument) => function
                .reduce_with_dbi(arg.clone(), dbi)
                .apply(argument.reduce_with_dbi(arg, dbi)),
            Fst(pair) => pair
                .reduce_with_dbi(arg.clone(), dbi)
                .first()
                .reduce_with_dbi(arg, dbi),
            Snd(pair) => pair
                .reduce_with_dbi(arg.clone(), dbi)
                .second()
                .reduce_with_dbi(arg, dbi),
        }
    }
}

/// Type values.
pub type TVal = Val;

/// Non-redex, canonical values.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Val {
    /// Type universe.
    Type(Level),
    /// An empty sum.
    Bot(Level),
    /// Closure with parameter typed.
    /// For untyped closures, it can be represented as `Neut` directly.
    Lam(Closure),
    /// Pi-like types (dependent types).
    Dt(DtKind, Closure),
    /// Sum type literal.
    Sum(BTreeMap<String, TVal>),
    /// Sigma instance.
    Pair(Box<Self>, Box<Self>),
    Neut(Neutral),
}

impl Val {
    pub fn is_type(&self) -> bool {
        match self {
            Val::Type(_) => true,
            Val::Bot(_) => true,
            Val::Lam(_) => false,
            Val::Dt(_, _) => true,
            Val::Sum(_) => true,
            Val::Pair(_, _) => false,
            // In case it's neutral, we use `is_universe` on its type.
            Val::Neut(_) => false,
        }
    }

    pub fn is_universe(&self) -> bool {
        match self {
            Val::Type(_) => true,
            _ => false,
        }
    }

    pub fn try_into_sum(self) -> Result<BTreeMap<String, TVal>, Self> {
        match self {
            Val::Sum(variants) => Ok(variants),
            e => Err(e),
        }
    }

    pub fn pair(first: Self, second: Self) -> Self {
        Val::Pair(Box::new(first), Box::new(second))
    }

    pub fn var(index: DBI) -> Self {
        Val::Neut(Neutral::Var(index))
    }

    pub fn lam(arg_type: TVal, body: Self) -> Self {
        Val::Lam(Closure::new(arg_type, body))
    }

    pub fn axiom() -> Self {
        Self::axiom_with_uid(unsafe { next_uid() })
    }

    pub(crate) fn axiom_with_uid(uid: UID) -> Self {
        Val::Neut(Neutral::Axi(uid, None))
    }

    pub(crate) fn axiom_with_index(uid: UID, dbi: DBI) -> Self {
        Val::Neut(Neutral::Axi(uid, Some(dbi)))
    }

    pub fn app(function: Neutral, arg: Self) -> Self {
        Val::Neut(Neutral::App(Box::new(function), Box::new(arg)))
    }

    pub fn fst(pair: Neutral) -> Self {
        Val::Neut(Neutral::Fst(Box::new(pair)))
    }

    pub fn snd(pair: Neutral) -> Self {
        Val::Neut(Neutral::Snd(Box::new(pair)))
    }

    pub fn dependent_type(kind: DtKind, param_type: TVal, body: Self) -> Self {
        Val::Dt(kind, Closure::new(param_type, body))
    }

    pub fn pi(param_type: TVal, body: TVal) -> Self {
        Self::dependent_type(DtKind::Pi, param_type, body)
    }

    pub fn sig(param_type: TVal, body: TVal) -> Self {
        Self::dependent_type(DtKind::Sigma, param_type, body)
    }

    pub fn into_neutral(self) -> Result<Neutral, Self> {
        match self {
            Val::Neut(n) => Ok(n),
            e => Err(e),
        }
    }

    pub fn map_neutral<F: Fn(Neutral) -> Neutral + Copy>(self, f: F) -> Self {
        match self {
            Val::Neut(n) => Val::Neut(f(n)),
            Val::Pair(a, b) => Self::pair(a.map_neutral(f), b.map_neutral(f)),
            Val::Sum(v) => Val::Sum(v.into_iter().map(|(k, v)| (k, v.map_neutral(f))).collect()),
            Val::Lam(Closure { param_type, body }) => {
                Self::lam(param_type.map_neutral(f), body.map_neutral(f))
            }
            Val::Dt(kind, Closure { param_type, body }) => {
                Self::dependent_type(kind, param_type.map_neutral(f), body.map_neutral(f))
            }
            e => e,
        }
    }
}

impl Default for Val {
    fn default() -> Self {
        Self::axiom()
    }
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub param_type: Box<TVal>,
    pub body: Box<Val>,
}

impl Closure {
    pub fn new(param_type: TVal, body: Val) -> Self {
        Self {
            param_type: Box::new(param_type),
            body: Box::new(body),
        }
    }
}
