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
    pub fn nil() -> Self {
        LocalEnv_::Nil
    }

    pub fn cons(self, canonical: T) -> Self {
        LocalEnv_::Cons(Rc::new(self), Box::new(canonical))
    }

    pub fn project(&self, index: DBI) -> Option<&T> {
        match self {
            LocalEnv_::Nil => None,
            LocalEnv_::Cons(next, term) => {
                if index == 0 {
                    Some(term)
                } else {
                    Some(&next[index - 1])
                }
            }
        }
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
