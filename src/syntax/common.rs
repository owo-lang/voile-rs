use voile_util::loc::Ident;

/// Global reference indices.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct GI(pub usize);
uid_basic_operations_impl!(GI);

#[derive(Debug, Clone, Eq, PartialEq)]
/// Typed label
pub struct Labelled<Expr> {
    /// Label is an identifier
    pub label: Ident,
    /// The thing attached on this label
    pub expr: Expr,
}

impl<Expr> Labelled<Expr> {
    pub fn map_expr<Abs>(self, f: impl FnOnce(Expr) -> Abs) -> Labelled<Abs> {
        Labelled {
            label: self.label,
            expr: f(self.expr),
        }
    }
}
