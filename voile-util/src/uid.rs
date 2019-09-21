#[macro_export]
macro_rules! uid_basic_operations_impl {
    ($name:ident) => {
        impl std::ops::Add<usize> for $name {
            type Output = $name;

            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }

        impl std::ops::Add<u32> for $name {
            type Output = $name;

            fn add(self, rhs: u32) -> Self::Output {
                self.add(rhs as usize)
            }
        }

        impl std::ops::Add<i32> for $name {
            type Output = $name;

            fn add(self, rhs: i32) -> Self::Output {
                Self(((self.0 as i32) + rhs) as usize)
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

        impl $name {
            /// Successor.
            pub fn succ(mut self) -> Self {
                self.0 += 1;
                self
            }

            /// Predecessor.
            pub fn pred(self) -> Self {
                self.nat().unwrap()
            }

            /// Pattern matcher.
            pub fn nat(self) -> Option<Self> {
                if self.0 == 0 {
                    None
                } else {
                    Some(Self(self.0 - 1))
                }
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

/// Reset the unique-ID generator.
pub unsafe fn reset_uid_counter() {
    UID_COUNT = 0;
}

/// Unique-ID generation function.
pub unsafe fn next_uid() -> UID {
    let val = UID_COUNT;
    UID_COUNT += 1;
    UID(val)
}
