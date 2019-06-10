use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::DtKind::*;

use super::{Axiom, Closure, Neutral, Val};

impl Display for Neutral {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Neutral::*;
        match self {
            Var(dbi) => write!(f, "[{:?}]", dbi),
            // This might be conflict with other syntax.
            Ref(dbi) => write!(f, "[|{:?}|]", dbi),
            Axi(a) => a.fmt(f),
            App(fun, a) => write!(f, "({} {})", fun, a),
            Fst(p) => write!(f, "({}.1)", p),
            Snd(p) => write!(f, "({}.2)", p),
            Lift(levels, p) => write!(f, "(^[{:?}] {})", levels, p),
        }
    }
}

impl Display for Axiom {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Axiom::*;
        match self {
            Postulated(uid) => write!(f, "<{:?}>", uid),
            Generated(uid, dbi) => write!(f, "<{:?} {:?}>", uid, dbi),
            Unimplemented(uid, dbi) => write!(f, "[|{:?} {:?}|]", uid, dbi),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.body.fmt(f)
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Val::Type(l) => write!(f, "set{}", l),
            Val::Lam(clos) => write!(f, "(\\ {})", clos),
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
                if !started {
                    f.write_str("!")?;
                }
                Ok(())
            }
            Val::Dt(Pi, param_ty, clos) => write!(f, "({} -> {})", param_ty, clos),
            Val::Dt(Sigma, param_ty, clos) => write!(f, "({} * {})", param_ty, clos),
            Val::Pair(fst, snd) => write!(f, "({}, {})", fst, snd),
            Val::Neut(neut) => neut.fmt(f),
            Val::Cons(name, a) => write!(f, "(@{} {})", name, a),
        }
    }
}
