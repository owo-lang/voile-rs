use crate::syntax::common::{DtKind, Level, SyntaxInfo, DBI};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Abstract {
    Type(SyntaxInfo, Level),
    /// Bottom type
    Bot(SyntaxInfo),
    /// Global variable
    Var(SyntaxInfo, DBI),
    /// Local variable
    Local(SyntaxInfo, DBI),
    /// Meta variable
    Meta(SyntaxInfo),
    /// Construct call
    Cons(SyntaxInfo),
    /// Construct call
    ConsType(SyntaxInfo),
    /// Apply or Pipeline in surface
    App(Box<Self>, Box<Self>),
    /// Dependent Type type, (a -> b -> c) as Dt(DtKind::Pi, a, Dt(DtKind::Pi, b, c))
    Dt(DtKind, Box<Abstract>, Box<Abstract>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
    Sum(Vec<Self>),
}

/// type signature and value in abstract syntax
#[derive(Debug, Clone)]
pub enum AbstractDecl {
    Sign(Abstract),
    Impl(Abstract),
    Both(Abstract, Abstract),
}

pub type AbstractGlobalEnv = VecDbiEnv_<AbstractDecl>;
pub type VecDbiEnv_<T> = BTreeMap<DBI, T>;
