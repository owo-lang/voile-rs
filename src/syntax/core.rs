use crate::syntax::env::{Env_, GlobalEnv_, LocalEnv_, DBI};
use crate::syntax::parser::concrete::SyntaxInfo;

pub type Level = u32;
pub type Env = Env_<Canonical>;
pub type LocalEnv = LocalEnv_<Canonical>;
pub type GlobalEnv = GlobalEnv_<Canonical>;

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
    pub fn app(self, arg: Canonical) -> Canonical {
        match self {
            Term::Lam(closure) => closure.body.instantiate(arg),
            // TODO neutral value
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    pub fn eval(self, env: &Env) -> Canonical {
        match self {
            Term::Lam(closure) => Canonical::Lam(closure),
            Term::Type(level) => Canonical::Type(level),
            Term::Sig(visibility, closure) => Canonical::Sig(visibility, closure),
            Term::Pi(visibility, closure) => Canonical::Pi(visibility, closure),
            Term::App(function, argument) => function.ast.app(argument.eval(env)),
            Term::Pair(first, second) => {
                Canonical::Pair(Box::new(first.eval(env)), Box::new(second.eval(env)))
            }
            Term::Var(n) => env.local[n].clone(),
            Term::Ref(name) => env.global[&name].clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    /// Local variable, referred by de-bruijn index.
    Var(DBI),
    /// Global variable, referred by variable name.
    Ref(String),
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
    Pair(Box<TermInfo>, Box<TermInfo>),
    /// Function application.
    App(Box<TermInfo>, Box<TermInfo>),
}

/// Non-redex.
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
    Pair(Box<Canonical>, Box<Canonical>),
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub param_type: Box<Canonical>,
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
