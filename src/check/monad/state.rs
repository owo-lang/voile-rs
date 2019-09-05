use voile_util::uid::DBI;

use crate::syntax::common::GI;
use crate::syntax::core::{Val, ValInfo};

/// Typing context.
pub type Gamma = Vec<ValInfo>;

pub type MetaSolution = voile_util::meta::MetaSolution<Val>;
pub type MetaContext = voile_util::meta::MetaContext<Val>;

/// Type-checking state.
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
    /// Meta variable context. Always global.
    pub meta_context: MetaContext,
}

impl TCS {
    /// Create a new valid but unsolved meta variable,
    /// used for generating fresh metas during elaboration.
    pub fn fresh_meta(&mut self) -> Val {
        self.meta_context.fresh_meta(Val::meta)
    }

    pub fn local_type(&self, dbi: DBI) -> &ValInfo {
        &self.local_gamma[self.local_gamma.len() - dbi.0 - 1]
    }

    pub fn glob_type(&self, index: GI) -> &ValInfo {
        &self.gamma[index.0]
    }

    pub fn local_val(&self, dbi: DBI) -> &ValInfo {
        &self.local_env[self.local_env.len() - dbi.0 - 1]
    }

    pub fn glob_val(&self, index: GI) -> &ValInfo {
        &self.env[index.0]
    }

    pub fn local_is_type(&self, dbi: DBI) -> bool {
        self.local_val(dbi).ast.is_type() || self.local_type(dbi).ast.is_universe()
    }

    pub fn glob_is_type(&self, index: GI) -> bool {
        self.glob_val(index).ast.is_type() || self.glob_type(index).ast.is_universe()
    }

    pub fn pop_local(&mut self) {
        // Yes, this deserves a panic.
        self.local_gamma
            .pop()
            .expect("Unexpected empty local gamma");
        self.local_env.pop().expect("Unexpected empty local env");
    }

    pub fn glob_len(&self) -> usize {
        self.gamma.len()
    }

    pub fn local_len(&self) -> usize {
        self.local_env.len()
    }
}
