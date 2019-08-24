use crate::syntax::common::{Ident, Labelled, SyntaxInfo, VarRec};
use crate::syntax::level::Level;
use crate::util::vec1::Vec1;

pub type LabExpr = Labelled<Expr>;

/// Surface syntax tree node: Parameter.
///
/// It's a part of a pi-type or a sigma-type (if we have those syntax element).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    /// This field can be empty -- which indicates the parameter to be anonymous.
    /// Many `name`s means there are many params with same type.
    pub names: Vec<Ident>,
    /// Parameter type.
    pub ty: Expr,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Variable reference.
    Var(Ident),
    /// Constructor call.
    Cons(Ident),
    /// Explicit meta variable.
    Meta(Ident),
    /// Lift an expression many times.
    Lift(SyntaxInfo, u32, Box<Self>),
    /// Record projections.
    Proj(Box<Self>, Vec1<Ident>),
    /// `Type` literal, with levels.
    Type(SyntaxInfo, Level),
    /// Function application.<br/>
    /// Application operator, where `f a b c` is represented as `App(f, vec![a, b, c])`
    /// instead of `App(App(App(f, a), b), c)`.
    App(Box<Vec1<Self>>),
    /// Function composition.<br/>
    /// Pipeline operator, where `a |> b |> f` is represented as `Pipe(vec![a, b, f])`
    /// instead of `Pipe(Pipe(Pipe(f, a), b), c)`.
    Pipe(Box<Vec1<Self>>),
    /// Tuple constructor.<br/>
    /// Comma operator, where `a, b, c` is represented as `Tup(a, vec![b, c])`
    /// instead of `Tup(Tup(a, b), c)`.
    Tup(Box<Vec1<Self>>),
    /// Row-polymorphic types, either record types or variant types.
    RowPoly(SyntaxInfo, VarRec, Vec<LabExpr>, Option<Box<Self>>),
    /// Record literals.
    Rec(SyntaxInfo, Vec<LabExpr>, Option<Box<Self>>),
    /// Row-polymorphic kinds, either record types or variant kinds.
    RowKind(SyntaxInfo, VarRec, Vec<Ident>),
    /// Pi-type expression, where `a -> b -> c` is represented as `Pi(vec![a, b], c)`
    /// instead of `Pi(a, Pi(b, c))`.
    /// `a` and `b` here can introduce telescopes.
    Pi(Vec<Param>, Box<Self>),
    /// Sigma-type expression, where `a * b * c` is represented as `Sig(vec![a, b], c)`
    /// instead of `Sig(a, Sig(b, c))`.
    /// `a` and `b` here can introduce telescopes.
    Sig(Vec<Param>, Box<Self>),
    /// Case-chains.
    Cases(Vec1<Case>, Box<Self>),
    /// Anonymous function, aka lambda expression.
    Lam(SyntaxInfo, Vec<Ident>, Box<Self>),
}

/// One case in a case-split chain.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Case {
    pub label: Ident,
    pub abstraction: Ident,
    pub body: Box<Expr>,
}

impl Case {
    pub fn new(label: Ident, abstraction: Ident, body: Expr) -> Self {
        Self {
            label,
            abstraction,
            body: Box::new(body),
        }
    }
}

impl Expr {
    pub fn pi(params: Vec<Param>, expr: Self) -> Self {
        Expr::Pi(params, Box::new(expr))
    }

    pub fn lam(info: SyntaxInfo, params: Vec<Ident>, expr: Self) -> Self {
        Expr::Lam(info, params, Box::new(expr))
    }

    pub fn app(applied: Self, arguments: Vec<Self>) -> Self {
        Expr::App(Box::new(Vec1::new(applied, arguments)))
    }

    pub fn lift(info: SyntaxInfo, count: u32, target: Self) -> Self {
        Expr::Lift(info, count, Box::new(target))
    }

    pub fn pipe(first: Self, functions: Vec<Self>) -> Self {
        Expr::Pipe(Box::new(Vec1::new(first, functions)))
    }

    pub fn row_polymorphic_type(
        info: SyntaxInfo,
        labels: Vec<LabExpr>,
        kind: VarRec,
        rest: Option<Self>,
    ) -> Self {
        Expr::RowPoly(info, kind, labels, rest.map(Box::new))
    }

    pub fn record(info: SyntaxInfo, fields: Vec<LabExpr>, rest: Option<Self>) -> Self {
        Expr::Rec(info, fields, rest.map(Box::new))
    }

    pub fn sum(info: SyntaxInfo, labels: Vec<LabExpr>, rest: Option<Self>) -> Self {
        Self::row_polymorphic_type(info, labels, VarRec::Variant, rest)
    }

    pub fn rec(info: SyntaxInfo, labels: Vec<LabExpr>, rest: Option<Self>) -> Self {
        Self::row_polymorphic_type(info, labels, VarRec::Record, rest)
    }

    pub fn tup(first: Self, rest: Vec<Self>) -> Self {
        Expr::Tup(Box::new(Vec1::new(first, rest)))
    }

    pub fn sig(params: Vec<Param>, expr: Self) -> Self {
        Expr::Sig(params, Box::new(expr))
    }

    pub fn proj(expr: Self, projections: Vec1<Ident>) -> Self {
        Expr::Proj(Box::new(expr), projections)
    }

    pub fn cases(cases: Vec1<Case>, or: Self) -> Self {
        Expr::Cases(cases, Box::new(or))
    }
}

/// Indicates that whether a `Decl` is a type signature or an implementation.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum DeclKind {
    /// Implementation.
    Impl,
    /// Signature.
    Sign,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Pragma {
    name: Ident,
}

/// Surface syntax tree node: Declaration.
///
/// It can be a type signature, where there's a name and a type expression;
/// or an implementation, where there's a name and an expression body.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Decl {
    pub name: Ident,
    pub body: Expr,
    pub kind: DeclKind,
    // TODO more, like pragma
}
