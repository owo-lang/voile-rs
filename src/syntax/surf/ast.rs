use crate::syntax::common::{Level, SyntaxInfo};

/// Surface syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub info: SyntaxInfo,
}

/// Surface syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Var(SyntaxInfo),
    Type(SyntaxInfo, Level),
    App(Vec<Expression>),
    Pipe(Vec<Expression>),
    Sum(Vec<Expression>),
}

/// Surface syntax tree node: Declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration {
    /// Implementation.
    Impl(NamedExpression),
    /// Signature.
    Sign(NamedExpression),
}

/// Surface syntax tree node, like implementation or signature.
///
/// They're all like:
/// ```ignore
/// signature : Expression
/// implementation = Expression
/// ```
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NamedExpression {
    pub name: Identifier,
    pub body: Expression,
    // TODO more, like pragma
}
