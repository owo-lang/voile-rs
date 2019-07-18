use std::mem::swap;

/// Non-empty vector.
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Vec1<T> {
    head: T,
    tail: Vec<T>,
}

impl<T> From<T> for Vec1<T> {
    fn from(head: T) -> Self {
        Self::new(head, Default::default())
    }
}

impl<T> Vec1<T> {
    pub fn new(head: T, tail: Vec<T>) -> Self {
        Self { head, tail }
    }

    pub fn push(&mut self, new: T) {
        self.tail.push(new)
    }

    pub fn insert(&mut self, index: usize, mut new: T) {
        if index == 0 {
            swap(&mut new, &mut self.head);
            self.tail.insert(0, new)
        } else {
            self.tail.insert(index - 1, new)
        }
    }

    pub fn map<R>(self, mut f: impl FnMut(T) -> R) -> Vec1<R> {
        Vec1::new(f(self.head), self.tail.into_iter().map(f).collect())
    }

    pub fn try_map<E, R>(self, mut f: impl FnMut(T) -> Result<R, E>) -> Result<Vec1<R>, E> {
        Ok(Vec1::new(
            f(self.head)?,
            self.tail.into_iter().map(f).collect::<Result<_, _>>()?,
        ))
    }

    pub fn try_fold1<E>(self, mut f: impl FnMut(T, T) -> Result<T, E>) -> Result<T, E> {
        let mut iter = self.tail.into_iter();
        let mut accum = self.head;
        for x in iter {
            accum = f(accum, x)?;
        }
        Ok(accum)
    }

    pub fn fold1(self, f: impl FnMut(T, T) -> T) -> T {
        self.tail.into_iter().fold(self.head, f)
    }

    pub fn rev_fold1(self, f: impl FnMut(T, T) -> T) -> T {
        self.tail.into_iter().rev().fold(self.head, f)
    }
}

impl<T: Default> Default for Vec1<T> {
    fn default() -> Self {
        Self::from(T::default())
    }
}
