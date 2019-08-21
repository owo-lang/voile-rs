use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::PiSig::*;

use super::{Axiom, Closure, Neutral, Val, ValInfo};
use crate::syntax::common::VarRec;
use crate::syntax::core::Variants;

impl Display for Neutral {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Neutral::*;
        match self {
            Var(dbi) => write!(f, "[{}]", dbi),
            // This might be conflict with other syntax.
            Ref(dbi) => write!(f, "[|{}|]", dbi),
            Axi(a) => a.fmt(f),
            Meta(mi) => write!(f, "?{}", mi),
            App(fun, a) => {
                write!(f, "({}", fun)?;
                for x in a {
                    write!(f, " {}", x)?;
                }
                f.write_str(")")
            }
            Fst(p) => write!(f, "({}.1)", p),
            Snd(p) => write!(f, "({}.2)", p),
            Lift(levels, p) => write!(f, "(^[{:?}] {})", levels, p),
            Row(kind, variants, ext) => {
                match kind {
                    VarRec::Variant => f.write_str("Sum"),
                    VarRec::Record => f.write_str("Rec"),
                }?;
                f.write_str(" {")?;
                write_variants(f, variants, ":")?;
                write!(f, " | {}}}", ext)
            }
            Rec(fields, ext) => {
                f.write_str("{|")?;
                write_variants(f, fields, " =")?;
                write!(f, ", ... = {}|}}", ext)
            }
        }
    }
}

impl Display for Axiom {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Axiom::*;
        match self {
            Postulated(uid) => write!(f, "<{}>", uid),
            Generated(uid, dbi) => write!(f, "<{} {}>", uid, dbi),
            Unimplemented(uid, dbi) => write!(f, "[|{} {}|]", uid, dbi),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Closure::*;
        match self {
            Plain(body) => body.fmt(f),
        }
    }
}

impl Display for ValInfo {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} at {}", self.ast, self.info)
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Val::Type(l) => write!(f, "set{}", l),
            Val::RowKind(l, kind, labels) => {
                match kind {
                    VarRec::Variant => f.write_str("sum"),
                    VarRec::Record => f.write_str("rec"),
                }?;
                write!(f, "{} {{", l)?;
                let mut started = false;
                for label in labels {
                    if started {
                        f.write_str(", ")?;
                    } else {
                        started = true;
                    }
                    f.write_str(label)?;
                }
                f.write_str("}")
            }
            Val::Lam(clos) => write!(f, "(\\ {})", clos),
            Val::RowPoly(kind, variants) => {
                match kind {
                    VarRec::Variant => f.write_str("Sum"),
                    VarRec::Record => f.write_str("Rec"),
                }?;
                f.write_str(" {")?;
                write_variants(f, variants, ":")?;
                f.write_str("}")
            }
            Val::Rec(fields) => {
                f.write_str("{|")?;
                write_variants(f, fields, " =")?;
                f.write_str("|}")
            }
            Val::Dt(Pi, param_ty, clos) => write!(f, "({} -> {})", param_ty, clos),
            Val::Dt(Sigma, param_ty, clos) => write!(f, "({} * {})", param_ty, clos),
            Val::Pair(fst, snd) => write!(f, "({}, {})", fst, snd),
            Val::Neut(neut) => neut.fmt(f),
            Val::Cons(name, a) => write!(f, "(@{} {})", name, a),
        }
    }
}

fn write_variants(f: &mut Formatter, variants: &Variants, sep: &str) -> Result<(), Error> {
    let mut started = false;
    for (name, param) in variants {
        if started {
            f.write_str(", ")?;
        } else {
            started = true;
        }
        write!(f, "{}{} {}", name, sep, param)?;
    }
    Ok(())
}
