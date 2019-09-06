#[macro_export]
macro_rules! uid_basic_operations_impl {
    ($name:ident) => {
        impl std::ops::Add<usize> for $name {
            type Output = $name;

            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }

        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, rhs: $name) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }

        impl std::ops::AddAssign<usize> for $name {
            fn add_assign(&mut self, rhs: usize) {
                self.0 += rhs
            }
        }

        impl std::ops::AddAssign for $name {
            fn add_assign(&mut self, rhs: $name) {
                self.0 += rhs.0
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
                self.0.fmt(f)
            }
        }
    };
}

/// De Bruijn Indices. Checkout [Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index) if you
/// are curious but have no idea about it.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct DBI(pub usize);
uid_basic_operations_impl!(DBI);

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct UID(pub usize);
uid_basic_operations_impl!(UID);

/// Global reference indices.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct GI(pub usize);
uid_basic_operations_impl!(GI);

/// Unique-ID generator internal counter.
static mut UID_COUNT: usize = 0;

/// Unique-ID generation function.
pub unsafe fn next_uid() -> UID {
    let val = UID_COUNT;
    UID_COUNT += 1;
    UID(val)
}
