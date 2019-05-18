use std::collections::btree_map::BTreeMap;

use crate::syntax::common::{next_uid, DtKind, Level, DBI, UID};

pub type NamedEnv = BTreeMap<String, Term>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// This is primarily a private implementation-related API.
    /// Use at your own risk.
    fn reduce_with_dbi(self, arg: Term, dbi: DBI) -> Term;

    /// Instantiate `self` as a closure (possibly neutral terms) with
    /// a concrete argument.
    #[inline]
    fn instantiate(self, arg: Term) -> Term {
        self.reduce_with_dbi(arg, 0)
    }
}

impl Term {
    /// Just for evaluation during beta-reduction.
    pub fn apply(self, arg: Term) -> Term {
        match self {
            Term::Lam(closure) => closure.body.instantiate(arg),
            Term::Neut(n) => Term::app(n, arg),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    /// Just for evaluation during beta-reduction.
    pub fn first(self) -> Term {
        match self {
            Term::Pair(a, _) => *a,
            Term::Neut(n) => Term::fst(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    /// Just for evaluation during beta-reduction.
    pub fn second(self) -> Term {
        match self {
            Term::Pair(_, b) => *b,
            Term::Neut(n) => Term::snd(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }
}

impl RedEx for Term {
    fn reduce_with_dbi(self, arg: Term, dbi: DBI) -> Term {
        match self {
            Term::Pair(a, b) => Term::pair(
                a.reduce_with_dbi(arg.clone(), dbi),
                b.reduce_with_dbi(arg, dbi),
            ),
            Term::Neut(neutral_value) => neutral_value.reduce_with_dbi(arg, dbi),
            Term::Lam(Closure { param_type, body }) => Term::lam(
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
    Axi(UID),
    /// Function application.
    App(Box<Self>, Box<Term>),
    /// Projecting the first element of a pair.
    Fst(Box<Self>),
    /// Projecting the second element of a pair.
    Snd(Box<Self>),
}

impl Neutral {
    pub fn map_var(self, f: impl FnOnce(DBI) -> DBI) -> Self {
        match self {
            Neutral::Var(dbi) => Neutral::Var(f(dbi)),
            e => e,
        }
    }
}

impl RedEx for Neutral {
    fn reduce_with_dbi(self, arg: Term, dbi: DBI) -> Term {
        use self::Neutral::*;
        match self {
            Var(n) if dbi == n => arg,
            Var(n) => Term::var(n),
            Axi(i) => Term::axiom_with_value(i),
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

/// Non-redex.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    /// Type universe.
    Type(Level),
    /// An empty sum.
    Bot(Level),
    /// Closure.
    Lam(Closure),
    /// Pi-like types (dependent types).
    Dt(DtKind, Closure),
    /// Sigma instance.
    Pair(Box<Self>, Box<Self>),
    Neut(Neutral),
}

impl Term {
    pub fn is_type(&self) -> bool {
        match self {
            Term::Type(_) => true,
            Term::Bot(_) => true,
            Term::Lam(_) => false,
            Term::Dt(_, _) => true,
            Term::Pair(_, _) => false,
            // In case it's neutral, we use `is_universe` on its type.
            Term::Neut(_) => false,
        }
    }

    pub fn is_universe(&self) -> bool {
        match self {
            Term::Type(_) => true,
            _ => false,
        }
    }

    pub fn pair(first: Self, second: Self) -> Self {
        Term::Pair(Box::new(first), Box::new(second))
    }

    pub fn var(index: DBI) -> Self {
        Term::Neut(Neutral::Var(index))
    }

    pub fn lam(arg_type: Self, body: Self) -> Self {
        Term::Lam(Closure::new(arg_type, body))
    }

    pub fn axiom() -> Self {
        Self::axiom_with_value(unsafe { next_uid() })
    }

    pub(crate) fn axiom_with_value(uid: UID) -> Self {
        Term::Neut(Neutral::Axi(uid))
    }

    pub fn app(function: Neutral, arg: Self) -> Self {
        Term::Neut(Neutral::App(Box::new(function), Box::new(arg)))
    }

    pub fn fst(pair: Neutral) -> Self {
        Term::Neut(Neutral::Fst(Box::new(pair)))
    }

    pub fn snd(pair: Neutral) -> Self {
        Term::Neut(Neutral::Snd(Box::new(pair)))
    }

    pub fn dependent_type(kind: DtKind, closure: Closure) -> Self {
        Term::Dt(kind, closure)
    }

    pub fn pi(param_type: Term, body: Term) -> Self {
        Self::dependent_type(DtKind::Pi, Closure::new(param_type, body))
    }

    pub fn sig(param_type: Term, body: Term) -> Self {
        Self::dependent_type(DtKind::Sigma, Closure::new(param_type, body))
    }

    pub fn into_neutral(self) -> Result<Neutral, Self> {
        match self {
            Term::Neut(n) => Ok(n),
            e => Err(e),
        }
    }

    pub fn map_neutral(self, f: impl FnOnce(Neutral) -> Neutral) -> Self {
        match self {
            Term::Neut(n) => Term::Neut(f(n)),
            e => e,
        }
    }
}

impl Default for Term {
    fn default() -> Self {
        Self::axiom()
    }
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub param_type: Box<Term>,
    pub body: Box<Term>,
}

impl Closure {
    pub fn new(param_type: Term, body: Term) -> Self {
        Self {
            param_type: Box::new(param_type),
            body: Box::new(body),
        }
    }
}
