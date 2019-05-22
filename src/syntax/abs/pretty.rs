use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::DtKind::*;

use super::{Abs, AbsDecl};

impl Display for Abs {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Abs::Type(_, level) => write!(f, "set{:?}", level),
            Abs::Bot(_) => write!(f, "Bot"),
            Abs::Local(info, name, dbi) => write!(f, "{}[{:?},{:?}]", info.text, name.uid, dbi),
            Abs::Var(_, dbi) => write!(f, "<{:?}>", dbi),
            Abs::Meta(info) => write!(f, "?{}", info.text),
            Abs::Cons(_) => unimplemented!(),
            Abs::ConsType(_) => unimplemented!(),
            Abs::App(_, a, b) => write!(f, "({} {})", a, b),
            Abs::Dt(_, Pi, name, param, ret) => {
                write!(f, "(<{:?}> : {}) -> {}", name.uid, param, ret)
            }
            Abs::Dt(_, Sigma, name, fst, snd) => {
                write!(f, "(<{:?}> : {}) * {}", name.uid, fst, snd)
            }
            Abs::Lam(_, param_info, name, body) => {
                write!(f, "(\\{}[{:?}]. {})", param_info.text, name.uid, body)
            }
            Abs::Pair(_, a, b) => write!(f, "({}, {})", a, b),
            Abs::Fst(_, p) => write!(f, "({}.1)", p),
            Abs::Snd(_, p) => write!(f, "({}.2)", p),
            Abs::Sum(_, _) => unimplemented!(),
        }
    }
}

impl Display for AbsDecl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AbsDecl::Sign(abs) => write!(f, "{}", abs),
            AbsDecl::Impl(abs) => write!(f, "_ : {}", abs),
            AbsDecl::Both(ty, val) => write!(f, "{} : {}", val, ty),
        }
    }
}
