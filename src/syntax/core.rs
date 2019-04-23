use super::env::{DbiEnv_, NamedEnv_};
use crate::syntax::common::{DtKind, Level, ParamKind, SyntaxInfo, DBI};

pub type DbiEnv = DbiEnv_<Term>;
pub type NamedEnv = NamedEnv_<Term>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TermInfo {
    pub ast: Term,
    pub info: SyntaxInfo,
}

impl TermInfo {
    pub fn new(ast: Term, info: SyntaxInfo) -> Self {
        Self { ast, info }
    }

    pub fn reduce(self, env: &DbiEnv) -> Term {
        self.ast.reduce(env)
    }

    /// Because in `reduce`, what actually moved is `self.ast`, not whole `self`.
    pub fn reduce_cloned(&self, env: &DbiEnv) -> Term {
        self.ast.clone().reduce(env)
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

    pub fn reduce(self, env: &DbiEnv) -> Term {
        match self {
            Term::Pair(a, b) => Term::pair(a.reduce(env), b.reduce(env)),
            Term::Neut(neutral_value) => neutral_value.reduce(env),
            // Cannot reduce
            e => e,
        }
    }
}

/// Irreducible because of the presence of generated value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Neutral {
    /// Local variable, referred by de-bruijn index.
    /// `None` indicates that this is a postulated value.
    Gen(Option<DBI>),
    /// Function application.
    App(Box<Neutral>, Box<Term>),
    /// Projecting the first element of a pair.
    Fst(Box<Neutral>),
    /// Projecting the second element of a pair.
    Snd(Box<Neutral>),
}

impl Neutral {
    pub fn reduce(self, env: &DbiEnv) -> Term {
        use crate::syntax::core::Neutral::*;
        match self {
            Gen(n) => n.map_or_else(Term::mock, |n| {
                env.project(n).cloned().unwrap_or_else(|| Term::gen(n))
            }),
            App(function, argument) => {
                let argument = argument.reduce(env);
                function.reduce(env).apply(argument)
            }
            Fst(pair) => pair.reduce(env).first().reduce(env),
            Snd(pair) => pair.reduce(env).second().reduce(env),
        }
    }
}

/// Non-redex.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    /// Type universe.
    Type(Level),
    /// An empty sum.
    Bot,
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

    pub fn gen(index: DBI) -> Self {
        Term::Neut(Neutral::Gen(Some(index)))
    }

    pub fn mock() -> Self {
        Term::Neut(Neutral::Gen(None))
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

    pub fn into_info(self, syntax_info: SyntaxInfo) -> TermInfo {
        TermInfo::new(self, syntax_info)
    }
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub param_type: Box<Term>,
    pub body: ClosureBody,
}

/// The instantiatable part of a closure.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClosureBody {
    pub body: Box<TermInfo>,
    pub env: DbiEnv,
}

impl ClosureBody {
    pub fn instantiate(self, arg: Term) -> Term {
        let env = self.env.cons(arg);
        self.body.reduce(&env)
    }
}
