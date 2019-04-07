use pest::Span;

/// Trivial information about the concrete syntax items.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SyntaxInfo {
    pub text: String,
    pub start: usize,
    pub end: usize,
}

impl<'a> From<Span<'a>> for SyntaxInfo {
    fn from(span: Span) -> Self {
        SyntaxInfo {
            text: span.as_str().to_string(),
            start: span.start(),
            end: span.end(),
        }
    }
}

/// Concrete syntax tree node: Identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub info: SyntaxInfo,
}

/// Concrete syntax tree node: Expression.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Var(SyntaxInfo),
    // TODO more
}

/// Concrete syntax tree node: Declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration {
    /// Implementation.
    Impl(NamedExpression),
    /// Signature.
    Sign(NamedExpression),
}

/// Concrete syntax tree node, like implementation or signature.
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
