use std::cmp::max;
use std::collections::BTreeMap;

use crate::syntax::common::{next_uid, DtKind, Level, DBI, UID};

pub type Variants = BTreeMap<String, TVal>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// This is primarily a private implementation-related API.
    /// Use at your own risk.
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val;
}

/// Expression with universe level (which means they can be lifted).
pub trait LiftEx: Sized {
    /// Lift the level of `self`.
    fn lift(self, levels: Level) -> Self;

    /// Calculate the level of `self`,
    /// like a normal value will have level 0,
    /// a type expression will have level 1 (or higher).
    fn level(&self) -> Level;
}

pub trait AxiomEx: Sized {
    fn generated_to_var(self) -> Self;
    fn unimplemented_to_glob(self) -> Self;
}

impl Val {
    /**
    $$
    \\newcommand{\\inst}[0]{\\texttt{inst}}
    \\newcommand{\\app}[0]{\\texttt{app}}
    \\begin{alignedat}{1}
    \\app(\\lambda C, o) &= \\inst(C, o) \\\\
    \\app([k], o) &= [k\ o]
    \\end{alignedat}
    $$
    Just for evaluation during beta-reduction.
    */
    pub fn apply(self, arg: Val) -> Val {
        match self {
            Val::Lam(closure) => closure.instantiate(arg),
            Val::Neut(n) => Val::app(n, arg),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    /**
    $$
    \\newcommand{\\first}[0]{\\texttt{first}}
    \\newcommand{\\second}[0]{\\texttt{second}}
    \\begin{alignedat}{1}
    \\first(n, m) &= n \\\\
    \\first([k]) &= [k\ .1] \\\\
    \\end{alignedat}
    $$
    Just for evaluation during beta-reduction.
    */
    pub fn first(self) -> Val {
        match self {
            Val::Pair(a, _) => *a,
            Val::Neut(n) => Val::fst(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    /**
    \\newcommand{\\first}[0]{\\texttt{first}}
    \\newcommand{\\second}[0]{\\texttt{second}}
    \\begin{alignedat}{1}
    \\second(n, m) &= m \\\\
    \\second([k]) &= [k\ .2]
    \\end{alignedat}
    Just for evaluation during beta-reduction.
    */
    pub fn second(self) -> Val {
        match self {
            Val::Pair(_, b) => *b,
            Val::Neut(n) => Val::snd(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    pub(crate) fn attach_dbi(self, dbi: DBI) -> Self {
        self.map_neutral(|neut: Neutral| {
            Val::Neut(neut.map_axiom(|a| {
                Neutral::Axi(match a {
                    Axiom::Postulated(uid) => Axiom::Generated(uid, dbi),
                    e => e,
                })
            }))
        })
    }
}

impl AxiomEx for Val {
    fn generated_to_var(self) -> Self {
        self.map_neutral(|neut| Val::Neut(neut.generated_to_var()))
    }

    fn unimplemented_to_glob(self) -> Self {
        self.map_neutral(|neut| Val::Neut(neut.unimplemented_to_glob()))
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
            Val::Lam(Closure { body }) => Val::lam(body.reduce_with_dbi(arg, dbi + 1)),
            Val::Dt(kind, param_type, Closure { body }) => Val::dependent_type(
                kind,
                param_type.reduce_with_dbi(arg.clone(), dbi),
                body.reduce_with_dbi(arg, dbi + 1),
            ),
            Val::Sum(variants) => Val::Sum(
                variants
                    .into_iter()
                    .map(|(name, ty)| (name, ty.reduce_with_dbi(arg.clone(), dbi)))
                    .collect(),
            ),
            Val::Cons(name, a) => Self::cons(name, a.reduce_with_dbi(arg, dbi)),
            Val::Type(n) => Val::Type(n),
        }
    }
}

impl LiftEx for TVal {
    fn lift(self, levels: Level) -> Val {
        match self {
            Val::Type(l) => Val::Type(l + levels),
            Val::Lam(closure) => Val::Lam(closure.lift(levels)),
            Val::Dt(kind, param_type, closure) => Val::Dt(
                kind,
                Box::new(param_type.lift(levels)),
                closure.lift(levels),
            ),
            Val::Sum(variants) => Val::Sum(
                variants
                    .into_iter()
                    .map(|(name, e)| (name, e.lift(levels)))
                    .collect(),
            ),
            Val::Cons(name, e) => Val::cons(name, e.lift(levels)),
            Val::Pair(l, r) => Val::pair(l.lift(levels), r.lift(levels)),
            Val::Neut(neut) => Val::Neut(neut.lift(levels)),
        }
    }

    fn level(&self) -> Level {
        match self {
            Val::Type(level) => *level + 1,
            Val::Sum(variants) => variants.iter().map(|(_, v)| v.level()).max().unwrap_or(0),
            Val::Dt(_, param_ty, closure) => max(param_ty.level(), closure.level()),
            Val::Lam(closure) => closure.level(),
            Val::Neut(neut) => neut.level(),
            Val::Pair(l, r) => max(l.level(), r.level()),
            Val::Cons(_, e) => e.level(),
        }
    }
}

/// Irreducible because of the presence of generated value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Neutral {
    /// Local variable, referred by de-bruijn index.
    Var(DBI),
    /// Global variable, referred by index. Needed for recursive definitions.
    Ref(DBI),
    /// Lifting self to a higher/lower level.
    Lift(Level, Box<Self>),
    /// Postulated value, aka axioms.
    Axi(Axiom),
    /// Function application.
    App(Box<Self>, Box<Val>),
    /// Projecting the first element of a pair.
    Fst(Box<Self>),
    /// Projecting the second element of a pair.
    Snd(Box<Self>),
}

/// Postulated value, aka axioms.
/// If the `Option<DBI>` value is `None`, then it's really postulated.
/// Otherwise it should be a generated lambda parameter.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Axiom {
    /// Functions without implementation.
    Postulated(UID),
    /// Lambda parameters during type-checking.
    /// (usually will be replaced with `Val::var` after the expression is type-checked).
    Generated(UID, DBI),
    /// Usages of definitions when they're not yet implemented.
    /// (usually will be replaced with `Val::glob` after implemented).
    Unimplemented(UID, DBI),
}

impl Axiom {
    pub fn unique_id(&self) -> UID {
        use Axiom::*;
        match self {
            Postulated(uid) | Generated(uid, ..) | Unimplemented(uid, ..) => *uid,
        }
    }
}

impl Neutral {
    pub fn map_axiom<F: Fn(Axiom) -> Self + Copy>(self, f: F) -> Self {
        use Neutral::*;
        match self {
            Axi(a) => f(a),
            App(fun, a) => App(
                Box::new(fun.map_axiom(f)),
                Box::new(a.map_neutral(|n| Val::Neut(n.map_axiom(f)))),
            ),
            Fst(p) => Fst(Box::new(p.map_axiom(f))),
            Snd(p) => Snd(Box::new(p.map_axiom(f))),
            Var(n) => Var(n),
            Ref(n) => Ref(n),
            Lift(levels, expr) => Lift(levels, Box::new(expr.map_axiom(f))),
        }
    }
}

impl AxiomEx for Neutral {
    fn generated_to_var(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(|a| match a {
            Postulated(..) | Unimplemented(..) => Axi(a),
            Generated(_, dbi) => Var(dbi),
        })
    }

    fn unimplemented_to_glob(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(|a| match a {
            Postulated(..) | Generated(..) => Axi(a),
            Unimplemented(_, dbi) => Ref(dbi),
        })
    }
}

impl RedEx for Neutral {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        use Neutral::*;
        match self {
            Var(n) if dbi == n => arg.attach_dbi(dbi),
            Var(n) => Val::var(n),
            Ref(n) => Val::glob(n),
            Axi(a) => Val::Neut(Axi(a)),
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
            Lift(levels, neut) => neut.reduce_with_dbi(arg, dbi).lift(levels),
        }
    }
}

impl LiftEx for Neutral {
    fn lift(self, levels: Level) -> Self {
        use self::Neutral::*;
        match self {
            Lift(n, expr) => Lift(n + levels, expr),
            e => Lift(levels, Box::new(e)),
        }
    }

    fn level(&self) -> Level {
        use self::Neutral::*;
        match self {
            Lift(n, expr) => expr.level() + *n,
            // Level is zero by default
            Var(..) | Axi(..) | Ref(..) => 0,
            Fst(expr) => expr.level(),
            Snd(expr) => expr.level(),
            App(f, a) => max(f.level(), a.level()),
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
    /// Closure with parameter typed.
    /// For untyped closures, it can be represented as `Neut` directly.
    Lam(Closure),
    /// Pi-like types (dependent types), with parameter explicitly typed.
    Dt(DtKind, Box<Self>, Closure),
    /// Sum type literal.
    Sum(Variants),
    /// Constructor invocation.
    Cons(String, Box<Self>),
    /// Sigma instance.
    Pair(Box<Self>, Box<Self>),
    Neut(Neutral),
}

impl Val {
    pub fn is_type(&self) -> bool {
        use Val::*;
        match self {
            Type(..) | Dt(..) | Sum(..) => true,
            // In case it's neutral, we use `is_universe` on its type.
            Lam(..) | Cons(..) | Pair(..) | Neut(..) => false,
        }
    }

    pub fn is_universe(&self) -> bool {
        match self {
            Val::Type(_) => true,
            _ => false,
        }
    }

    pub fn try_into_sum(self) -> Result<Variants, Self> {
        match self {
            Val::Sum(variants) => Ok(variants),
            e => Err(e),
        }
    }

    pub fn pair(first: Self, second: Self) -> Self {
        Val::Pair(Box::new(first), Box::new(second))
    }

    pub fn cons(name: String, param: Self) -> Self {
        Val::Cons(name, Box::new(param))
    }

    pub fn lift(levels: Level, expr: Neutral) -> Self {
        Val::Neut(Neutral::Lift(levels, Box::new(expr)))
    }

    pub fn var(index: DBI) -> Self {
        Val::Neut(Neutral::Var(index))
    }

    pub fn lam(body: Self) -> Self {
        Val::Lam(Closure::new(body))
    }

    pub fn bot() -> TVal {
        Val::Sum(Default::default())
    }

    pub fn glob(index: DBI) -> Self {
        Val::Neut(Neutral::Ref(index))
    }

    pub fn axiom() -> Self {
        Self::postulate(unsafe { next_uid() })
    }

    pub(crate) fn postulate(uid: UID) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Postulated(uid)))
    }

    pub(crate) fn generate(uid: UID, dbi: DBI) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Generated(uid, dbi)))
    }

    pub fn unimplemented(dbi: DBI) -> Self {
        let axiom = Axiom::Unimplemented(unsafe { next_uid() }, dbi);
        Val::Neut(Neutral::Axi(axiom))
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

    pub fn dependent_type(kind: DtKind, param_type: TVal, body: TVal) -> TVal {
        Val::Dt(kind, Box::new(param_type), Closure::new(body))
    }

    pub fn pi(param_type: TVal, body: TVal) -> TVal {
        Self::dependent_type(DtKind::Pi, param_type, body)
    }

    pub fn sig(param_type: TVal, body: TVal) -> TVal {
        Self::dependent_type(DtKind::Sigma, param_type, body)
    }

    pub fn into_neutral(self) -> Result<Neutral, Self> {
        match self {
            Val::Neut(n) => Ok(n),
            e => Err(e),
        }
    }

    pub fn map_neutral<F: Fn(Neutral) -> Self + Copy>(self, f: F) -> Self {
        match self {
            Val::Neut(n) => f(n),
            Val::Pair(a, b) => Self::pair(a.map_neutral(f), b.map_neutral(f)),
            Val::Sum(v) => Val::Sum(v.into_iter().map(|(k, v)| (k, v.map_neutral(f))).collect()),
            Val::Lam(Closure { body }) => Self::lam(body.map_neutral(f)),
            Val::Dt(kind, param_type, Closure { body }) => {
                Self::dependent_type(kind, param_type.map_neutral(f), body.map_neutral(f))
            }
            Val::Cons(name, a) => Self::cons(name, a.map_neutral(f)),
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
    pub body: Box<Val>,
}

impl Closure {
    pub fn new(body: Val) -> Self {
        Self {
            body: Box::new(body),
        }
    }

    pub fn instantiate(self, arg: Val) -> Val {
        self.body.reduce_with_dbi(arg, 0)
    }

    pub fn instantiate_cloned(&self, arg: Val) -> Val {
        self.body.clone().reduce_with_dbi(arg, 0)
    }
}

impl LiftEx for Closure {
    fn lift(self, levels: Level) -> Self {
        Self::new(self.body.lift(levels))
    }

    fn level(&self) -> Level {
        self.body.level()
    }
}
