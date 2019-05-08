use crate::syntax::common::{DtKind, Level, SyntaxInfo, DBI};

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
    App(SyntaxInfo, Box<Self>, Box<Self>),
    /// Dependent Type type, (a -> b -> c) as Dt(DtKind::Pi, a, Dt(DtKind::Pi, b, c))
    Dt(SyntaxInfo, DtKind, Box<Self>, Box<Self>),
    Lam(SyntaxInfo, Box<Self>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
    Sum(SyntaxInfo, Vec<Self>),
}

impl Abs {
    pub fn syntax_info(&self) -> &SyntaxInfo {
        match self {
            Abs::Type(info, _) => info,
            Abs::Bot(info) => info,
            Abs::Var(info, _) => info,
            Abs::Local(info, _) => info,
            Abs::Meta(info) => info,
            Abs::Cons(info) => info,
            Abs::ConsType(info) => info,
            Abs::App(info, _, _) => info,
            Abs::Dt(info, _, _, _) => info,
            Abs::Pair(info, _, _) => info,
            Abs::Fst(info, _) => info,
            Abs::Snd(info, _) => info,
            Abs::Sum(info, _) => info,
            Abs::Lam(info, _) => info,
        }
    }

    pub fn dependent_type(info: SyntaxInfo, kind: DtKind, a: Self, b: Self) -> Self {
        Abs::Dt(info, kind, Box::new(a), Box::new(b))
    }

    pub fn app(info: SyntaxInfo, function: Self, argument: Self) -> Self {
        Abs::App(info, Box::new(function), Box::new(argument))
    }

    pub fn fst(info: SyntaxInfo, of: Self) -> Self {
        Abs::Fst(info, Box::new(of))
    }

    pub fn snd(info: SyntaxInfo, of: Self) -> Self {
        Abs::Snd(info, Box::new(of))
    }

    pub fn pair(info: SyntaxInfo, first: Self, second: Self) -> Self {
        Abs::Pair(info, Box::new(first), Box::new(second))
    }

    pub fn pi(info: SyntaxInfo, input: Self, output: Self) -> Self {
        Self::dependent_type(info, DtKind::Pi, input, output)
    }

    pub fn sig(info: SyntaxInfo, first: Self, second: Self) -> Self {
        Self::dependent_type(info, DtKind::Sigma, first, second)
    }
}

/// type signature and value in abstract syntax
#[derive(Debug, Clone)]
pub enum AbsDecl {
    Sign(Abs),
    Impl(Abs),
    /// `Sign` and `Impl`
    Both(Abs, Abs),
    /// Postulated value, like lambda parameters.
    None,
}
