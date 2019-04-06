use crate::syntax::env::{Env_, GlobalEnv_, LocalEnv_, DBI};

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
pub struct Term {
    pub info: TermInfo,
}

impl Term {
    pub fn new(info: TermInfo) -> Self {
        Self { info }
    }

    pub fn eval(self, mut env: Env) -> Canonical {
        match self.info {
            TermInfo::Canonical(e) => e,
            TermInfo::Var(n) => env.local[n].clone(),
            TermInfo::Ref(name) => env.global.remove(&name).unwrap(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TermInfo {
    /// Local variable, referred by de-bruijn index.
    Var(DBI),
    /// Global variable, referred by variable name.
    Ref(String),
    /// Directly a canonical normal value which cannot reduce itself
    Canonical(Canonical),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Canonical {
    Type(Level),
    /// Closure.
    Lambda(Closure),
    /// Pi type. Since it affects type-checking translation, the visibility of the parameter
    /// need to be specified.
    Pi(Visib, Closure),
    /// Sigma type, ditto.
    Sigma(Visib, Closure),
    Pair(Box<Term>, Box<Term>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub param_type: Box<Canonical>,
    pub body: Box<Term>,
    pub env: Env,
}

impl Closure {
    pub fn instantiate(self, param: Canonical) -> Canonical {
        self.body.eval(Env::up_local(self.env, param))
    }
}
