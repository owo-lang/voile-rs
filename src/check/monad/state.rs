use crate::syntax::common::DBI;
use crate::syntax::core::ValInfo;

/// Typing context.
pub type Gamma = Vec<ValInfo>;

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
    pub fn local_type(&self, dbi: DBI) -> ValInfo {
        self.local_gamma[self.local_gamma.len() - dbi - 1].clone()
    }

    pub fn glob_type(&self, dbi: DBI) -> ValInfo {
        self.gamma[dbi].clone()
    }

    pub fn local_val(&self, dbi: DBI) -> ValInfo {
        self.local_env[self.local_env.len() - dbi - 1].clone()
    }

    pub fn glob_val(&self, dbi: DBI) -> ValInfo {
        self.env[dbi].clone()
    }

    pub fn local_is_type(&self, dbi: DBI) -> bool {
        self.local_val(dbi).ast.is_type() || self.local_type(dbi).ast.is_universe()
    }

    pub fn glob_is_type(&self, dbi: DBI) -> bool {
        self.glob_val(dbi).ast.is_type() || self.glob_type(dbi).ast.is_universe()
    }

    pub fn pop_local(&mut self) {
        // Yes, this deserves a panic.
        self.local_gamma
            .pop()
            .expect("Unexpected empty local gamma");
        self.local_env.pop().expect("Unexpected empty local env");
    }
}
