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

    pub fn append(&mut self, mut new: Vec1<T>) {
        self.push(new.head);
        self.append_vec(&mut new.tail)
    }

    pub fn append_vec(&mut self, new: &mut Vec<T>) {
        self.tail.append(new)
    }

    pub fn tail(&self) -> &T {
        self.tail.last().unwrap_or(&self.head)
    }

    pub fn tail_mut(&mut self) -> &mut T {
        self.tail.last_mut().unwrap_or(&mut self.head)
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
        let mut accum = self.head;
        for x in self.tail.into_iter() {
            accum = f(accum, x)?;
        }
        Ok(accum)
    }

    pub fn fold1(self, f: impl FnMut(T, T) -> T) -> T {
        self.tail.into_iter().fold(self.head, f)
    }

    pub fn fold<R>(self, init: R, mut f: impl FnMut(R, T) -> R) -> R {
        let init = f(init, self.head);
        self.tail.into_iter().fold(init, f)
    }

    pub fn rev_fold1(self, f: impl FnMut(T, T) -> T) -> T {
        self.tail.into_iter().rev().fold(self.head, f)
    }

    pub fn rev_fold<R>(self, init: R, mut f: impl FnMut(R, T) -> R) -> R {
        let init = f(init, self.head);
        self.tail.into_iter().rev().fold(init, f)
    }
}

impl<T: Default> Default for Vec1<T> {
    fn default() -> Self {
        Self::from(T::default())
    }
}
