use crate::syntax::common::{Level, ParamKind, SyntaxInfo};

/// Surface syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub info: SyntaxInfo,
}

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
    /// Indicates that whether this `Param` is implicit or explicit.
    pub kind: ParamKind,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Variable reference
    Var(Ident),
    /// Constructor call
    Cons(Ident),
    /// Type of a constructor call
    ConsType(Ident),
    /// Empty type
    Bot(Ident),
    /// Explicit meta variable
    Meta(Ident),
    /// `Type` literal, with levels
    Type(SyntaxInfo, Level),
    /// Function application.<br/>
    /// Application operator, where `f a b` is represented as `App(vec![f, a, b])`
    /// instead of `App(App(f, a), b)`.
    App(Vec<Expr>),
    /// Function composition.<br/>
    /// Pipeline operator, where `a |> b |> f` is represented as `Pipe(vec![a, b, f])`
    /// instead of `Pipe(Pipe(a, b), f)`.
    Pipe(Vec<Expr>),
    /// Tuple constructor.<br/>
    /// Comma operator, where `a, b, c` is represented as `Tup(vec![a, b, c])`
    /// instead of `Tup(Tup(a, b), c)`.
    Tup(Vec<Expr>),
    /// Type-sum operator.
    Sum(Vec<Expr>),
    /// Pi-type expression, where `a -> b -> c` is represented as `Pi(vec![a, b], c)`
    /// instead of `Pi(a, Pi(b, c))`.
    /// `a` and `b` here can introduce telescopes.
    Pi(Vec<Param>, Box<Expr>),
    /// Sigma-type expression, where `a * b * c` is represented as `Sig(vec![a, b], c)`
    /// instead of `Sig(a, Sig(b, c))`.
    /// `a` and `b` here can introduce telescopes.
    Sig(Vec<Param>, Box<Expr>),
    /// Anonymous function, aka lambda expression.
    Lam(Vec<Ident>, Box<Expr>),
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
