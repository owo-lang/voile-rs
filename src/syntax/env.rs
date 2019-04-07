use std::collections::BTreeMap;
use std::ops::Index;
use std::rc::Rc;

pub type DBI = usize;
pub type GlobalEnv_<T> = BTreeMap<String, T>;

/// Local context, can be captured inside of a lambda.
/// Represented as a list, projected by de-bruijn indices.
#[derive(Debug, Clone)]
pub enum LocalEnv_<T> {
    Nil,
    Cons(Rc<Self>, Box<T>),
}

impl<T> LocalEnv_<T> {
    pub fn up(self, canonical: T) -> Self {
        LocalEnv_::Cons(Rc::new(self), Box::new(canonical))
    }
}

impl<T> Index<DBI> for LocalEnv_<T> {
    type Output = T;

    /// Projecting from this environment.
    fn index(&self, index: DBI) -> &Self::Output {
        match self {
            LocalEnv_::Nil => panic!("DeBruijn index overflow."),
            LocalEnv_::Cons(next, term) => {
                if index == 0 {
                    term
                } else {
                    &next[index - 1]
                }
            }
        }
    }
}

impl<T> Eq for LocalEnv_<T> {}
impl<T> PartialEq for LocalEnv_<T> {
    /// We don't do comparison for `Env`s.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
