use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::DtKind::*;

use super::{Axiom, Closure, Neutral, Val};

impl Display for Neutral {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Neutral::Var(dbi) => write!(f, "[{:?}]", dbi),
            Neutral::Axi(Axiom::Postulated(uid)) => write!(f, "<{:?}>", uid),
            Neutral::Axi(Axiom::Generated(uid, dbi)) => write!(f, "<{:?} {:?}>", uid, dbi),
            Neutral::App(fun, a) => write!(f, "({} {})", fun, a),
            Neutral::Fst(p) => write!(f, "({}.1)", p),
            Neutral::Snd(p) => write!(f, "({}.2)", p),
            Neutral::Lift(levels, p) => write!(f, "(^[{:?}] {})", levels, p),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} -> {}", self.param_type, self.body)
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Val::Type(l) => write!(f, "set{:?}", l),
            Val::Lam(clos) => write!(f, "({} . {})", clos.param_type, clos.body),
            Val::Sum(variants) => {
                let mut started = false;
                for (name, param) in variants {
                    if started {
                        f.write_str(" + ")?;
                    } else {
                        started = true;
                    }
                    write!(f, "'{} {}", name, param)?;
                }
                if variants.is_empty() {
                    f.write_str("!")?;
                }
                Ok(())
            }
            Val::Dt(Pi, clos) => write!(f, "({} -> {})", clos.param_type, clos.body),
            Val::Dt(Sigma, clos) => write!(f, "({} * {})", clos.param_type, clos.body),
            Val::Pair(fst, snd) => write!(f, "({}, {})", fst, snd),
            Val::Neut(neut) => neut.fmt(f),
            Val::Cons(name, a) => write!(f, "(@{} {})", name, a),
        }
    }
}
