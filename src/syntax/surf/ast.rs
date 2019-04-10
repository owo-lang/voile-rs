use crate::syntax::common::{Level, SyntaxInfo};

/// Surface syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident {
    pub info: SyntaxInfo,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Var(SyntaxInfo),
    Cons(SyntaxInfo),
    Meta(SyntaxInfo),
    Type(SyntaxInfo, Level),
    App(Vec<Expr>),
    Pipe(Vec<Expr>),
    Sum(Vec<Expr>),
}

/// Surface syntax tree node: Declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Decl {
    /// Implementation.
    Impl(NamedExpr),
    /// Signature.
    Sign(NamedExpr),
}

/// Surface syntax tree node, like implementation or signature.
///
/// They're all like:
/// ```ignore
/// signature : Expression
/// implementation = Expression
/// ```
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NamedExpr {
    pub name: Ident,
    pub body: Expr,
    // TODO more, like pragma
}
