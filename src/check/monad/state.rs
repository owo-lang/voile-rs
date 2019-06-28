use crate::syntax::common::{DBI, GI, MI};
use crate::syntax::core::{Val, ValInfo};
use std::hint::unreachable_unchecked;
use std::mem::swap;

/// Typing context.
pub type Gamma = Vec<ValInfo>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MetaSolution {
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

impl Default for MetaSolution {
    fn default() -> Self {
        MetaSolution::Unsolved
    }
}

impl MetaSolution {
    pub fn solved(val: Val) -> Self {
        MetaSolution::Solved(Box::new(val))
    }
}

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
    meta_context: Vec<MetaSolution>,
}

impl TCS {
    /// Submit a solution to a meta variable to the context.
    pub fn solve_meta(&mut self, meta_index: MI, solution: Val) {
        let meta_solution = &mut self.meta_context[meta_index.0];
        debug_assert_eq!(meta_solution, &mut MetaSolution::Unsolved);
        *meta_solution = MetaSolution::solved(solution);
    }

    pub fn meta_solutions(&self) -> &[MetaSolution] {
        &self.meta_context
    }

    /// Add many unsolved metas to the context.
    pub fn expand_with_fresh_meta(&mut self, meta_count: MI) {
        debug_assert!(self.meta_context.len() <= meta_count.0);
        self.meta_context
            .resize_with(meta_count.0, Default::default);
    }

    /// Create a new valid but unsolved meta variable,
    /// used for generating fresh metas during elaboration.
    pub fn fresh_meta(&mut self) -> Val {
        let meta = Val::meta(MI(self.meta_context.len()));
        self.meta_context.push(MetaSolution::Unsolved);
        meta
    }

    pub fn take_meta(&mut self, meta_index: MI) -> Option<Val> {
        let x = &mut self.meta_context[meta_index.0];
        match x {
            MetaSolution::Solved(_) => {
                let mut inlined = MetaSolution::Inlined;
                swap(&mut inlined, x);
                match inlined {
                    MetaSolution::Solved(solution) => Some(*solution),
                    // It is too obvious that these cases are unreachable.
                    _ => unsafe { unreachable_unchecked() },
                }
            }
            MetaSolution::Unsolved => None,
            MetaSolution::Inlined => None,
        }
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
