use crate::syntax::common::DBI;
use crate::syntax::core::TermInfo;

/// Typing context.
pub type Gamma = Vec<TermInfo>;

#[derive(Debug, Clone, Default)]
pub struct TCS {
    /// Global value context.
    pub env: Gamma,
    /// Local value context.
    pub local_env: Gamma,
    /// Global typing context.
    pub gamma: Gamma,
    /// Local typing context.
    pub local_gamma: Gamma,
}

impl TCS {
    pub fn local_type(&self, dbi: DBI) -> &TermInfo {
        &self.local_gamma[self.local_gamma.len() - dbi - 1]
    }

    pub fn glob_type(&self, dbi: DBI) -> &TermInfo {
        &self.gamma[dbi]
    }

    pub fn local_val(&self, dbi: DBI) -> &TermInfo {
        &self.local_env[self.local_env.len() - dbi - 1]
    }

    pub fn glob_val(&self, dbi: DBI) -> &TermInfo {
        &self.env[dbi]
    }

    pub fn pop_local(&mut self) {
        // Yes, this deserves a panic.
        self.local_gamma
            .pop()
            .expect("Unexpected empty local gamma");
        self.local_env.pop().expect("Unexpected empty local env");
    }
}
