use crate::syntax::env::{Env_, GlobalEnv_, LocalEnv_, DBI};
use crate::syntax::parser::concrete::SyntaxInfo;

pub type Level = u32;
pub type Env = Env_<Canonical>;
pub type LocalEnv = LocalEnv_<Term>;
pub type GlobalEnv = GlobalEnv_<Term>;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Visib {
    Explicit,
    Implicit,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TermInfo {
    pub ast: Term,
    pub info: SyntaxInfo,
}

impl TermInfo {
    pub fn new(ast: Term, info: SyntaxInfo) -> Self {
        Self { ast, info }
    }

    pub fn eval(self, env: &Env) -> Canonical {
        self.ast.eval(env)
    }
}

impl Term {
    pub fn apply(self, arg: Canonical) -> Canonical {
        match self {
            Term::Cano(canonical_value) => canonical_value.apply(arg),
            // TODO neutral value
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    pub fn eval(self, env: &Env) -> Canonical {
        match self {
            Term::Cano(canonical_value) => canonical_value,
            Term::Neut(neutral_value) => neutral_value.eval(env),
        }
    }
}

/// Irreducible because of the presence of generated value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Neutral {
    /// Local variable, referred by de-bruijn index.
    Gen(DBI),
    /// Function application.
    App(Box<Neutral>, Box<Term>),
}

impl Neutral {
    pub fn eval(self, env: &Env) -> Canonical {
        use crate::syntax::core::Neutral::*;
        match self {
            Gen(n) => env.local[n].clone(),
            App(function, argument) => {
                let argument = argument.eval(env);
                function.eval(env).apply(argument)
            }
        }
    }
}

/// Non-redex.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Cano(Canonical),
    Neut(Neutral),
}

impl Term {
    pub fn ty(level: Level) -> Self {
        Term::Cano(Canonical::Type(level))
    }

    pub fn lam(closure: Closure) -> Self {
        Term::Cano(Canonical::Lam(closure))
    }

    pub fn pi(visibility: Visib, closure: Closure) -> Self {
        Term::Cano(Canonical::Pi(visibility, closure))
    }

    pub fn sig(visibility: Visib, closure: Closure) -> Self {
        Term::Cano(Canonical::Sig(visibility, closure))
    }

    pub fn pair(first: Self, second: Self) -> Self {
        Term::Cano(Canonical::Pair(Box::new(first), Box::new(second)))
    }

    pub fn gen(index: DBI) -> Self {
        Term::Neut(Neutral::Gen(index))
    }

    pub fn app(function: Neutral, arg: Term) -> Self {
        Term::Neut(Neutral::App(Box::new(function), Box::new(arg)))
    }
}

/// Irreducible because it cannot.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Canonical {
    /// Type universe.
    Type(Level),
    /// Closure.
    Lam(Closure),
    /// Pi type. Since it affects type-checking translation, the visibility of the parameter
    /// need to be specified.
    Pi(Visib, Closure),
    /// Sigma type, ditto.
    Sig(Visib, Closure),
    /// Sigma instance.
    Pair(Box<Term>, Box<Term>),
}

impl Canonical {
    /// Just for evaluation during beta-reduction.
    pub fn apply(self, arg: Canonical) -> Canonical {
        match self {
            Canonical::Lam(closure) => closure.body.instantiate(arg),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
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
    pub env: Env,
}

impl ClosureBody {
    pub fn instantiate(self, arg: Canonical) -> Canonical {
        let env = self.env.up_local(arg);
        self.body.eval(&env)
    }
}
