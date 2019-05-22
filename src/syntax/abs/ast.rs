use std::cmp::Ordering;

use crate::syntax::common::*;

/// Unique identifier.
#[derive(Debug, Clone, Copy, Eq, Ord)]
pub struct Name {
    pub uid: UID,
}

impl Name {
    pub fn from(uid: UID) -> Self {
        Self { uid }
    }
}

impl Default for Name {
    fn default() -> Self {
        Self::from(unsafe { next_uid() })
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.uid.partial_cmp(&other.uid)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Abs {
    Type(SyntaxInfo, Level),
    /// Bottom type
    Bot(SyntaxInfo),
    /// Local variable
    Local(SyntaxInfo, Name, DBI),
    /// Global variable
    Var(SyntaxInfo, DBI),
    /// Meta variable
    Meta(SyntaxInfo),
    /// Constructor call
    Cons(SyntaxInfo),
    /// One element inside of a sum type
    ConsType(SyntaxInfo),
    /// Apply or Pipeline in surface
    App(SyntaxInfo, Box<Self>, Box<Self>),
    /// Dependent Type, `(a -> b -> c)` as `Dt(DtKind::Pi, a, Dt(DtKind::Pi, b, c))`
    Dt(SyntaxInfo, DtKind, Name, Box<Self>, Box<Self>),
    /// The first `SyntaxInfo` is the syntax info of this whole lambda,
    /// while the second is about its parameter
    Lam(SyntaxInfo, SyntaxInfo, Name, Box<Self>),
    Pair(SyntaxInfo, Box<Self>, Box<Self>),
    Fst(SyntaxInfo, Box<Self>),
    Snd(SyntaxInfo, Box<Self>),
    Sum(SyntaxInfo, Vec<Self>),
}

impl ToSyntaxInfo for Abs {
    fn syntax_info(&self) -> &SyntaxInfo {
        match self {
            Abs::Type(info, _) => info,
            Abs::Bot(info) => info,
            Abs::Local(info, _, _) => info,
            Abs::Var(info, _) => info,
            Abs::Meta(info) => info,
            Abs::Cons(info) => info,
            Abs::ConsType(info) => info,
            Abs::App(info, _, _) => info,
            Abs::Dt(info, _, _, _, _) => info,
            Abs::Pair(info, _, _) => info,
            Abs::Fst(info, _) => info,
            Abs::Snd(info, _) => info,
            Abs::Sum(info, _) => info,
            Abs::Lam(info, _, _, _) => info,
        }
    }
}

impl Abs {
    pub fn dependent_type(info: SyntaxInfo, kind: DtKind, name: Name, a: Self, b: Self) -> Self {
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

    pub fn lam(whole_info: SyntaxInfo, param_info: SyntaxInfo, name: Name, body: Self) -> Self {
        Abs::Lam(whole_info, param_info, name, Box::new(body))
    }

    pub fn pair(info: SyntaxInfo, first: Self, second: Self) -> Self {
        Abs::Pair(info, Box::new(first), Box::new(second))
    }

    pub fn pi(info: SyntaxInfo, name: Name, input: Self, output: Self) -> Self {
        Self::dependent_type(info, DtKind::Pi, name, input, output)
    }

    pub fn sig(info: SyntaxInfo, name: Name, first: Self, second: Self) -> Self {
        Self::dependent_type(info, DtKind::Sigma, name, first, second)
    }
}

/// Type signature and body implementation,
/// with abstract syntax.
#[derive(Debug, Clone)]
pub enum AbsDecl {
    Sign(Abs),
    Impl(Abs),
    /// `Sign` and `Impl`
    Both(Abs, Abs),
}
