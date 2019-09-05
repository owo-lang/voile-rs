use std::fmt::{Display, Error, Formatter};
use std::mem::swap;

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

pub struct MetaContext<Val>(Vec<MetaSolution<Val>>);

impl<Val> Default for MetaContext<Val> {
    fn default() -> Self {
        MetaContext(Vec::new())
    }
}

impl<Val> MetaContext<Val> {
    pub fn solutions(&self) -> &Vec<MetaSolution<Val>> {
        &self.0
    }

    pub fn mut_solutions(&mut self) -> &mut Vec<MetaSolution<Val>> {
        &mut self.0
    }

    /// Add many unsolved metas to the context.
    pub fn expand_with_fresh_meta(&mut self, meta_count: MI) {
        debug_assert!(self.solutions().len() <= meta_count.0);
        self.mut_solutions()
            .resize_with(meta_count.0, Default::default);
    }

    /// Create a new valid but unsolved meta variable,
    /// used for generating fresh metas during elaboration.
    pub fn fresh_meta(&mut self, new_meta: impl FnOnce(MI) -> Val) -> Val {
        let meta = new_meta(MI(self.solutions().len()));
        self.mut_solutions().push(MetaSolution::Unsolved);
        meta
    }

    pub fn take_meta(&mut self, meta_index: MI) -> Option<Val> {
        let x = &mut self.mut_solutions()[meta_index.0];
        match x {
            MetaSolution::Solved(_) => {
                let mut inlined = MetaSolution::Inlined;
                swap(&mut inlined, x);
                match inlined {
                    MetaSolution::Solved(solution) => Some(*solution),
                    // It is too obvious that these cases are unreachable.
                    _ => unreachable!(),
                }
            }
            MetaSolution::Unsolved => None,
            MetaSolution::Inlined => None,
        }
    }
}

impl<Val: std::fmt::Debug + Eq> MetaContext<Val> {
    /// Submit a solution to a meta variable to the context.
    pub fn solve_meta(&mut self, meta_index: MI, solution: Val) {
        let meta_solution = &mut self.mut_solutions()[meta_index.0];
        debug_assert_eq!(meta_solution, &mut MetaSolution::Unsolved);
        *meta_solution = MetaSolution::solved(solution);
    }
}

impl<Val: Display> Display for MetaContext<Val> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use MetaSolution::*;
        for (index, solution) in self.solutions().iter().enumerate() {
            match solution {
                Solved(solution) => writeln!(f, "{}: {}", index, solution),
                Unsolved => writeln!(f, "{}: ???", index),
                Inlined => writeln!(f, "<inlined out>"),
            }?;
        }
        Ok(())
    }
}
