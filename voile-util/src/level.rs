use std::cmp::{max, Ordering};
use std::fmt::{Display, Error, Formatter};
use std::ops::{Add, Sub};

/// Level, can be inferred or user-specified.
#[derive(Debug, Copy, Clone, Ord, Eq, PartialEq)]
pub enum Level {
    Omega,
    Num(u32),
}

impl Level {
    pub fn max(self, other: Self) -> Self {
        use Level::*;
        match (self, other) {
            (Num(a), Num(b)) => Num(max(a, b)),
            _ => Omega,
        }
    }

    pub fn map(self, f: impl FnOnce(u32) -> u32) -> Self {
        self.and_then(|a| Level::Num(f(a)))
    }

    pub fn and_then(self, f: impl FnOnce(u32) -> Self) -> Self {
        use Level::*;
        match self {
            Omega => Omega,
            Num(n) => f(n),
        }
    }
}

/// Internal API, public only because it's used in public traits' internal APIs.
/// Produced during level calculation.<br/>
/// `Some(Level)` -- level of non-recursive definitions.<br/>
/// `None` -- level of self-reference.<br/>
/// Trying to lift this will result in omega, otherwise it should be computed as 0 level.
pub type LevelCalcState = Option<Level>;

/// Expression with universe level (which means they can be lifted).
pub trait LiftEx: Sized {
    /// Lift the level of `self`.
    fn lift(self, levels: u32) -> Self;

    /// Internal API, for code sharing only.
    fn calc_level(&self) -> LevelCalcState;

    /// Calculate the level of `self`,
    /// like a normal value will have level 0,
    /// a type expression will have level 1 (or higher).
    fn level(&self) -> Level {
        self.calc_level().unwrap_or_default()
    }
}

impl From<u32> for Level {
    fn from(n: u32) -> Self {
        Level::Num(n)
    }
}

impl From<usize> for Level {
    fn from(n: usize) -> Self {
        From::from(n as u32)
    }
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Level::Omega => f.write_str("\u{03C9}"),
            Level::Num(n) => n.fmt(f),
        }
    }
}

impl Default for Level {
    fn default() -> Self {
        From::from(0u32)
    }
}

impl PartialOrd for Level {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Level::*;
        match (self, other) {
            (Omega, Omega) => Some(Ordering::Equal),
            (Omega, Num(..)) => Some(Ordering::Greater),
            (Num(..), Omega) => Some(Ordering::Less),
            (Num(a), Num(b)) => a.partial_cmp(b),
        }
    }
}

impl Add for Level {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use Level::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a + b),
            _ => Omega,
        }
    }
}

impl Add<u32> for Level {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        self.map(|a| a + rhs)
    }
}

impl Sub<u32> for Level {
    type Output = Self;

    fn sub(self, rhs: u32) -> Self::Output {
        self.map(|a| a - rhs)
    }
}
