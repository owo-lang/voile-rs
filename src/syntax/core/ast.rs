use std::collections::btree_map::BTreeMap;

use crate::syntax::common::{DtKind, Level, DBI};

pub type NamedEnv = BTreeMap<String, Term>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// Instantiate `self` as a closure (possibly neutral terms) with
    /// a concrete argument.
    fn reduce(self, arg: Term, dbi: DBI) -> Term;

    #[inline]
    fn instantiate(self, arg: Term) -> Term {
        self.reduce(arg, 0)
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
    fn reduce(self, arg: Term, dbi: DBI) -> Term {
        match self {
            Term::Pair(a, b) => Term::pair(a.reduce(arg.clone(), dbi), b.reduce(arg, dbi)),
            Term::Neut(neutral_value) => neutral_value.reduce(arg, dbi),
            Term::Lam(Closure { param_type, body }) => Term::lam(
                param_type.reduce(arg.clone(), dbi),
                body.reduce(arg, dbi + 1),
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
    Axi,
    /// Function application.
    App(Box<Self>, Box<Term>),
    /// Projecting the first element of a pair.
    Fst(Box<Self>),
    /// Projecting the second element of a pair.
    Snd(Box<Self>),
}

impl RedEx for Neutral {
    fn reduce(self, arg: Term, dbi: DBI) -> Term {
        use self::Neutral::*;
        match self {
            Var(n) if dbi == n => arg,
            Var(n) => Term::var(n),
            Axi => Term::axiom(),
            App(function, argument) => function
                .reduce(arg.clone(), dbi)
                .apply(argument.reduce(arg, dbi)),
            Fst(pair) => pair.reduce(arg.clone(), dbi).first().reduce(arg, dbi),
            Snd(pair) => pair.reduce(arg.clone(), dbi).second().reduce(arg, dbi),
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
        Term::Neut(Neutral::Axi)
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

    pub fn into_neutral(self) -> Option<Neutral> {
        match self {
            Term::Neut(n) => Some(n),
            _ => None,
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
