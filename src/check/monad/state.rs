use crate::check::monad::TCM;
use crate::syntax::common::{SyntaxInfo, DBI};
use crate::syntax::core::{DbiEnv, Term};

/// Gamma item.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GammaItem {
    /// This is not a de Bruijn index, but it should be of the type `DBI`.
    /// It refers to its index in `TCS::env`.
    pub dbi: DBI,
    /// Because it's a name binding, there should be source code location.
    pub location: SyntaxInfo,
    /// The type of this name.
    pub r#type: Term,
}

/// Typing context.
pub type Gamma = Vec<GammaItem>;

#[derive(Debug, Clone, Default)]
pub struct TCS {
    /// Global+local value context.
    pub env: DbiEnv,
    /// This is not a de Bruijn index, but it should be of the type `DBI`.
    /// It represents the size of `env`.
    pub env_size: DBI,
    /// Global typing context.
    pub gamma: Gamma,
}

impl TCS {
    pub fn modify_env(mut self, f: impl FnOnce(DbiEnv) -> DbiEnv) -> Self {
        self.env = f(self.env);
        self
    }

    pub fn try_modify_env(mut self, f: impl FnOnce(DbiEnv) -> TCM<DbiEnv>) -> TCM<Self> {
        self.env = f(self.env)?;
        Ok(self)
    }
}
