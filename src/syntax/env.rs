use crate::syntax::common::DBI;
use std::collections::BTreeMap;
use std::ops::Index;
use std::rc::Rc;

pub type GlobalEnv_<T> = BTreeMap<String, T>;

/// Local context, can be captured inside of a lambda.
/// Represented as a list, projected by de-bruijn indices.
#[derive(Debug, Clone)]
pub enum LocalEnv_<T> {
    Nil,
    Cons(Rc<Self>, Box<T>),
}

impl<T> LocalEnv_<T> {
    pub fn is_empty(&self) -> bool {
        self == &LocalEnv_::Nil
    }

    pub fn len(&self) -> usize {
        match self {
            LocalEnv_::Nil => 0,
            LocalEnv_::Cons(me, _) => 1 + me.len(),
        }
    }

    pub fn cons(self, canonical: T) -> Self {
        LocalEnv_::cons_rc(Rc::new(self), canonical)
    }

    pub fn cons_rc(me: Rc<Self>, canonical: T) -> Self {
        LocalEnv_::Cons(me, Box::new(canonical))
    }

    /// Return `Ok(self)` means substitution succeeded, `Err(self)` means failed.
    pub fn substitute_at(&self, index: DBI, new: T) -> Result<Self, Self> {
        use self::LocalEnv_::*;
        match self {
            Nil => Err(Nil),
            Cons(next, _old_term) => {
                if index == 0 {
                    Ok(LocalEnv_::cons_rc(next.clone(), new))
                } else {
                    next.substitute_at(index - 1, new)
                }
            }
        }
    }

    pub fn project(&self, index: DBI) -> Option<&T> {
        match self {
            LocalEnv_::Nil => None,
            LocalEnv_::Cons(next, term) => {
                if index == 0 {
                    Some(term)
                } else {
                    next.project(index - 1)
                }
            }
        }
    }
}

impl<T> Default for LocalEnv_<T> {
    fn default() -> Self {
        LocalEnv_::Nil
    }
}

impl<T> Index<DBI> for LocalEnv_<T> {
    type Output = T;

    /// Projecting from this environment.
    fn index(&self, index: DBI) -> &Self::Output {
        self.project(index).expect("DeBruijn index overflow.")
    }
}

impl<T> Eq for LocalEnv_<T> {}
impl<T> PartialEq for LocalEnv_<T> {
    /// We don't do comparison for `Env`s.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
