/// Meta variable indices (they're resolved as global reference).
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct MI(pub usize);
uid_basic_operations_impl!(MI);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MetaSolution<Val> {
    /// Solved meta.
    ///
    /// Boxed to make the variable smaller.
    Solved(Box<Val>),
    /// Not yet solved meta.
    Unsolved,
    /// This may probably be unused and we'll see.
    /// If so, it's gonna be deleted.
    Inlined,
}

impl<Val> Default for MetaSolution<Val> {
    fn default() -> Self {
        MetaSolution::Unsolved
    }
}

impl<Val> MetaSolution<Val> {
    pub fn solved(val: Val) -> Self {
        MetaSolution::Solved(Box::new(val))
    }
}
