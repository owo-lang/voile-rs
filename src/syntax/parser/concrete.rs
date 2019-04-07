/// Trivial information about the concrete syntax items.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SyntaxInfo {
    pub text: String,
    pub start: i32,
    pub end: i32,
}

/// Concrete syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub info: SyntaxInfo,
}

/// Concrete syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Var,
    // TODO more
}

/// Concrete syntax tree node: Declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Declaration {
    pub name: Identifier,
    // TODO more
}
