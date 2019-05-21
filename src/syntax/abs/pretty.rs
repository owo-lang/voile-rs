use super::Abs;
use crate::syntax::common::DtKind::*;
use std::fmt::{Display, Error, Formatter};

impl Display for Abs {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Abs::Type(_, level) => write!(f, "Type{:?}", level),
            Abs::Bot(_) => write!(f, "Bot"),
            Abs::Local(_, name, dbi) => write!(f, "[{:?},{:?}]", name.uid, dbi),
            Abs::Var(_, dbi) => write!(f, "<{:?}>", dbi),
            Abs::Meta(_) => unimplemented!(),
            Abs::Cons(_) => unimplemented!(),
            Abs::ConsType(_) => unimplemented!(),
            Abs::App(_, a, b) => write!(f, "({} {})", a, b),
            Abs::Dt(_, Pi, name, param, ret) => {
                write!(f, "(<{:?}> : {}) -> {}", name.uid, param, ret)
            }
            Abs::Dt(_, Sigma, name, fst, snd) => {
                write!(f, "(<{:?}> : {}) * {}", name.uid, fst, snd)
            }
            Abs::Lam(_, _, name, body) => write!(f, "\\{:?}. {}", name.uid, body),
            Abs::Pair(_, a, b) => write!(f, "({}, {})", a, b),
            Abs::Fst(_, p) => write!(f, "({}.1)", p),
            Abs::Snd(_, p) => write!(f, "({}.2)", p),
            Abs::Sum(_, _) => unimplemented!(),
        }
    }
}
