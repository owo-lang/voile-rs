use crate::syntax::common::{DtKind, Level, SyntaxInfo, DBI};
use crate::syntax::env::VecDbiEnv_;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Abs {
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
    Dt(DtKind, Box<Self>, Box<Self>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
    Sum(Vec<Self>),
}

impl Abs {
    pub fn dependent_type(kind: DtKind, a: Self, b: Self) -> Self {
        Abs::Dt(kind, Box::new(a), Box::new(b))
    }

    pub fn app(function: Self, argument: Self) -> Self {
        Abs::App(Box::new(function), Box::new(argument))
    }

    pub fn fst(syntax_info: SyntaxInfo, of: Self) -> Self {
        Abs::Fst(syntax_info, Box::new(of))
    }

    pub fn snd(syntax_info: SyntaxInfo, of: Self) -> Self {
        Abs::Snd(syntax_info, Box::new(of))
    }

    pub fn pair(syntax_info: SyntaxInfo, first: Self, second: Self) -> Self {
        Abs::Pair(syntax_info, Box::new(first), Box::new(second))
    }

    pub fn pi(input: Self, output: Self) -> Self {
        Self::dependent_type(DtKind::Pi, input, output)
    }

    pub fn sig(first: Self, second: Self) -> Self {
        Self::dependent_type(DtKind::Sigma, first, second)
    }
}

/// type signature and value in abstract syntax
#[derive(Debug, Clone)]
pub enum AbsDecl {
    Sign(SyntaxInfo, Abs),
    Impl(SyntaxInfo, Abs),
    /// `Sign` and `Impl`
    Both(SyntaxInfo, Abs, SyntaxInfo, Abs),
}

pub type AbsGlobEnv = VecDbiEnv_<AbsDecl>;
