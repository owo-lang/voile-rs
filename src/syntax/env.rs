use crate::syntax::common::DBI;
use std::collections::BTreeMap;
use std::ops::Index;
use std::rc::Rc;

pub type NamedEnv_<T> = BTreeMap<String, T>;
pub type VecDbiEnv_<T> = BTreeMap<DBI, T>;

/// Local context, can be captured inside of a lambda.
/// Represented as a list, projected by de-bruijn indices.
#[derive(Debug, Clone)]
pub enum DbiEnv_<T> {
    Nil,
    Cons(Rc<Self>, Box<T>),
}

impl<T> DbiEnv_<T> {
    pub fn is_empty(&self) -> bool {
        self == &DbiEnv_::Nil
    }

    pub fn len(&self) -> usize {
        match self {
            DbiEnv_::Nil => 0,
            DbiEnv_::Cons(me, _) => 1 + me.len(),
        }
    }

    pub fn cons(self, canonical: T) -> Self {
        DbiEnv_::cons_rc(Rc::new(self), canonical)
    }

    pub fn cons_rc(me: Rc<Self>, canonical: T) -> Self {
        DbiEnv_::Cons(me, Box::new(canonical))
    }

    /// Return `Ok(self)` means substitution succeeded, `Err(self)` means failed.
    pub fn substitute_at(&self, index: DBI, new: T) -> Result<Self, Self> {
        use self::DbiEnv_::*;
        match self {
            Nil => Err(Nil),
            Cons(next, _old_term) => {
                if index == 0 {
                    Ok(DbiEnv_::cons_rc(next.clone(), new))
                } else {
                    next.substitute_at(index - 1, new)
                }
            }
        }
    }

    pub fn project(&self, index: DBI) -> Option<&T> {
        match self {
            DbiEnv_::Nil => None,
            DbiEnv_::Cons(next, term) => {
                if index == 0 {
                    Some(term)
                } else {
                    next.project(index - 1)
                }
            }
        }
    }
}

impl<T> Default for DbiEnv_<T> {
    fn default() -> Self {
        DbiEnv_::Nil
    }
}

impl<T> Index<DBI> for DbiEnv_<T> {
    type Output = T;

    /// Projecting from this environment.
    fn index(&self, index: DBI) -> &Self::Output {
        self.project(index).expect("DeBruijn index overflow.")
    }
}

impl<T> Eq for DbiEnv_<T> {}
impl<T> PartialEq for DbiEnv_<T> {
    /// We don't do comparison for `Env`s.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
