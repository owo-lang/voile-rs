use std::ops::Index;

type Level = u32;
type DBI = u32;

/// Context
#[derive(Debug, Clone)]
pub enum Env {
    Nil,
    Cons(Box<Self>, Box<Term>),
}

impl Index<DBI> for Env {
    type Output = Term;

    /// Project from this environment
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TermInfo {
    Type(Level),
    /// Local variable, referred by de-bruijn index
    Var(DBI),
    /// Global variable, referred by variable name
    Ref(String),
    Lambda(LambdaInfo),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LambdaInfo {
    pub param_type: Box<Term>,
    pub visib: Visib,
    pub body: Box<Term>,
    pub env: Env,
}
