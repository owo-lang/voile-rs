/// Meta variable indices (they're resolved as global reference).
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct MI(pub usize);
uid_basic_operations_impl!(MI);
