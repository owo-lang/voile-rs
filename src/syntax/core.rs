use std::ops::Index;
use std::rc::Rc;

type Level = u32;
type DBI = usize;

/// Context, can be captured inside of a lambda
#[derive(Debug, Clone)]
pub enum Env {
    Nil,
    Cons(Rc<Self>, Box<Canonical>),
}

impl Env {
    pub fn up(self, canonical: Canonical) -> Self {
        Env::Cons(Rc::new(self), Box::new(canonical))
    }
}

impl Index<DBI> for Env {
    type Output = Canonical;

    /// Projecting from this environment.
    fn index(&self, index: DBI) -> &Self::Output {
        match self {
            Env::Nil => panic!("DeBruijn index overflow."),
            Env::Cons(next, term) => {
                if index == 0 {
                    term
                } else {
                    &next[index - 1]
                }
            }
        }
    }
}

impl Eq for Env {}
impl PartialEq for Env {
    /// We don't do comparison for `Env`s.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

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

    pub fn eval(self, env: Env) -> Canonical {
        match self.info {
            TermInfo::Canonical(e) => e,
            TermInfo::Var(n) => env[n].clone(),
            _ => unimplemented!(),
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
    pub param_type: Box<Term>,
    pub body: Box<Term>,
    pub env: Env,
}

impl Closure {
    pub fn instantiate(self, param: Canonical) -> Canonical {
        self.body.eval(Env::up(self.env, param))
    }
}
