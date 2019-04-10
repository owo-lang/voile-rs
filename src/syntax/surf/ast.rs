use crate::syntax::common::{Level, SyntaxInfo};

/// Surface syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub info: SyntaxInfo,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Var(Ident),
    Cons(Ident),
    Meta(Ident),
    Type(SyntaxInfo, Level),
    App(Vec<Expr>),
    Pipe(Vec<Expr>),
    Sum(Vec<Expr>),
}

/// Surface syntax tree node: Declaration.
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Decl {
    pub name: Ident,
    pub body: Expr,
    pub kind: DeclKind,
    // TODO more, like pragma
}
