use crate::syntax::common::*;
use crate::syntax::level::Level;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Abs {
    Type(SyntaxInfo, Level),
    /// Bottom type
    Bot(SyntaxInfo),
    /// Local variable
    Var(Ident, UID, DBI),
    /// Global variable
    Ref(Ident, DBI),
    /// Meta variable
    Meta(Ident),
    /// Lift an expression many times
    Lift(SyntaxInfo, u32, Box<Self>),
    /// Constructor call
    Cons(Ident),
    /// One variant inside of a sum type
    Variant(Ident),
    /// Apply or Pipeline in surface
    App(SyntaxInfo, Box<Self>, Box<Self>),
    /// Dependent Type, `(a -> b -> c)` as `Dt(DtKind::Pi, a, Dt(DtKind::Pi, b, c))`
    Dt(SyntaxInfo, DtKind, UID, Box<Self>, Box<Self>),
    /// The first `SyntaxInfo` is the syntax info of this whole lambda,
    /// while the second is about its parameter
    Lam(SyntaxInfo, Ident, UID, Box<Self>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
    Sum(SyntaxInfo, Vec<Self>),
}

impl ToSyntaxInfo for Abs {
    fn syntax_info(&self) -> SyntaxInfo {
        match self {
            Abs::Type(info, _)
            | Abs::Bot(info)
            | Abs::App(info, _, _)
            | Abs::Dt(info, _, _, _, _)
            | Abs::Pair(info, _, _)
            | Abs::Fst(info, _)
            | Abs::Snd(info, _)
            | Abs::Sum(info, _)
            | Abs::Lift(info, _, _)
            | Abs::Lam(info, _, _, _) => *info,
            Abs::Var(ident, _, _)
            | Abs::Ref(ident, _)
            | Abs::Meta(ident)
            | Abs::Cons(ident)
            | Abs::Variant(ident) => ident.info,
        }
    }
}

impl Abs {
    pub fn dependent_type(info: SyntaxInfo, kind: DtKind, name: UID, a: Self, b: Self) -> Self {
        Abs::Dt(info, kind, name, Box::new(a), Box::new(b))
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

    pub fn lam(whole_info: SyntaxInfo, param: Ident, name: UID, body: Self) -> Self {
        Abs::Lam(whole_info, param, name, Box::new(body))
    }

    pub fn pair(info: SyntaxInfo, first: Self, second: Self) -> Self {
        Abs::Pair(info, Box::new(first), Box::new(second))
    }

    pub fn lift(info: SyntaxInfo, lift_count: u32, expr: Self) -> Self {
        Abs::Lift(info, lift_count, Box::new(expr))
    }

    pub fn pi(info: SyntaxInfo, name: UID, input: Self, output: Self) -> Self {
        Self::dependent_type(info, DtKind::Pi, name, input, output)
    }

    pub fn sig(info: SyntaxInfo, name: UID, first: Self, second: Self) -> Self {
        Self::dependent_type(info, DtKind::Sigma, name, first, second)
    }
}

/// Type signature and body implementation,
/// with abstract syntax.
#[derive(Debug, Clone)]
pub enum AbsDecl {
    /// Signature.
    Sign(Abs, DBI),
    /// Function body without a signature.
    Decl(Abs),
    /// Function body with a signature.
    Impl(Abs, DBI),
}

impl ToSyntaxInfo for AbsDecl {
    fn syntax_info(&self) -> SyntaxInfo {
        use AbsDecl::*;
        match self {
            Sign(abs, ..) | Decl(abs) | Impl(abs, ..) => abs.syntax_info(),
        }
    }
}
