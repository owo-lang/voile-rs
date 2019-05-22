use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::DtKind::*;

use super::{Closure, Neutral, Term};

impl Display for Neutral {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Neutral::Var(dbi) => write!(f, "[{:?}]", dbi),
            Neutral::Axi(uid) => write!(f, "<{:?}>", uid),
            Neutral::App(fun, a) => write!(f, "({} {})", fun, a),
            Neutral::Fst(p) => write!(f, "({}.1)", p),
            Neutral::Snd(p) => write!(f, "({}.2)", p),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} -> {}", self.param_type, self.body)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Term::Type(l) => write!(f, "set{:?}", l),
            Term::Bot(l) => write!(f, "!{:?}", l),
            Term::Lam(clos) => write!(f, "({} . {})", clos.param_type, clos.body),
            Term::Dt(Pi, clos) => write!(f, "({} -> {})", clos.param_type, clos.body),
            Term::Dt(Sigma, clos) => write!(f, "({} * {})", clos.param_type, clos.body),
            Term::Pair(fst, snd) => write!(f, "({}, {})", fst, snd),
            Term::Neut(neut) => neut.fmt(f),
        }
    }
}
