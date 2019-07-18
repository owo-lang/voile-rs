use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::DtKind::*;

use super::{Abs, AbsDecl};

impl Display for Abs {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Abs::Type(_, level) => write!(f, "set{}", level),
            Abs::Bot(_) => write!(f, "Bot"),
            Abs::Var(info, name, dbi) => write!(f, "{}[{:?},{:?}]", info.text, name, dbi),
            Abs::Ref(_, dbi) => write!(f, "<{:?}>", dbi),
            Abs::Meta(_, mi) => write!(f, "?{:?}", mi),
            Abs::Cons(name) => write!(f, "@{}", name.text),
            Abs::Lift(_, levels, expr) => write!(f, "(^[{:?}] {})", levels, expr),
            Abs::App(_, a, b) => write!(f, "({} {})", a, b),
            Abs::Dt(_, Pi, name, param, ret) => write!(f, "(<{:?}> : {}) -> {}", name, param, ret),
            Abs::Dt(_, Sigma, name, fst, snd) => write!(f, "(<{:?}> : {}) * {}", name, fst, snd),
            Abs::Lam(_, param, name, body) => write!(f, "(\\{}[{:?}]. {})", param.text, name, body),
            Abs::Pair(_, a, b) => write!(f, "({}, {})", a, b),
            Abs::Fst(_, p) => write!(f, "({}.1)", p),
            Abs::Snd(_, p) => write!(f, "({}.2)", p),
            Abs::RowPoly(_, _, _, _) => unimplemented!(),
        }
    }
}

impl Display for AbsDecl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AbsDecl::Sign(abs, dbi) => write!(f, "[{}] {}", dbi, abs),
            AbsDecl::Decl(abs) => write!(f, "_ : {}", abs),
            AbsDecl::Impl(abs, ty_dbi) => write!(f, "{} : [{}]", abs, ty_dbi),
        }
    }
}
