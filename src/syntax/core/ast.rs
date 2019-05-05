use std::collections::btree_map::BTreeMap;

use crate::syntax::common::{DtKind, Level, ParamKind, DBI};

pub type NamedEnv = BTreeMap<String, Term>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// Instantiate `self` as a closure (possibly neutral terms) with
    /// a concrete argument.
    fn reduce(self, arg: Term) -> Term;

    /// Try my best to simplify this term out of nothing.
    fn bare_reduce(self) -> Term {
        self.reduce(Term::var(0))
    }
}

impl Term {
    /// Just for evaluation during beta-reduction.
    pub fn apply(self, arg: Term) -> Term {
        match self {
            Term::Lam(closure) => closure.body.reduce(arg),
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
    fn reduce(self, arg: Term) -> Term {
        match self {
            Term::Pair(a, b) => Term::pair(a.reduce(arg.clone()), b.reduce(arg)),
            Term::Neut(neutral_value) => neutral_value.reduce(arg),
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
    fn reduce(self, arg: Term) -> Term {
        use self::Neutral::*;
        match self {
            Var(0) => arg,
            Var(n) => Term::var(n - 1),
            Axi => Term::axiom(),
            App(function, argument) => function.reduce(arg.clone()).apply(argument.reduce(arg)),
            Fst(pair) => pair.reduce(arg.clone()).first().reduce(arg),
            Snd(pair) => pair.reduce(arg.clone()).second().reduce(arg),
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
    /// Pi-like types (dependent types). Since it affects type-checking translation, the visibility
    /// of the parameter need to be specified.
    Dt(ParamKind, DtKind, Closure),
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

    pub fn axiom() -> Self {
        Term::Neut(Neutral::Axi)
    }

    pub fn app(function: Neutral, arg: Term) -> Self {
        Term::Neut(Neutral::App(Box::new(function), Box::new(arg)))
    }

    pub fn fst(pair: Neutral) -> Self {
        Term::Neut(Neutral::Fst(Box::new(pair)))
    }

    pub fn snd(pair: Neutral) -> Self {
        Term::Neut(Neutral::Snd(Box::new(pair)))
    }

    pub fn dependent_type(visib: ParamKind, kind: DtKind, closure: Closure) -> Self {
        Term::Dt(visib, kind, closure)
    }

    pub fn pi(visibility: ParamKind, closure: Closure) -> Self {
        Self::dependent_type(visibility, DtKind::Pi, closure)
    }

    pub fn sig(visibility: ParamKind, closure: Closure) -> Self {
        Self::dependent_type(visibility, DtKind::Sigma, closure)
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
