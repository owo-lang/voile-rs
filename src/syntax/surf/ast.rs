use crate::syntax::common::{Level, SyntaxInfo};

/// Surface syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub info: SyntaxInfo,
}

/// Indicates that whether a `Param` is implicit or explicit.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ParamKind {
    Explicit,
    Implicit,
}

/// Surface syntax tree node: Parameter.
///
/// It's a part of a pi-type or a sigma-type (if we have those syntax element).
/// The `names` field can be empty -- this indicates the parameter to be anonymous.
/// There can be many `names`, which is treated as many params with same type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub names: Vec<Ident>,
    pub ty: Expr,
    pub kind: ParamKind,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Var(Ident),
    Cons(Ident),
    ConsType(Ident),
    Meta(Ident),
    Type(SyntaxInfo, Level),
    App(Vec<Expr>),
    Pipe(Vec<Expr>),
    Sum(Vec<Expr>),
    Pi(Vec<Param>, Box<Expr>),
}

/// Indicates that whether a `Decl` is a type signature or an implementation.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
