use std::fmt::{Display, Error, Formatter};

use crate::syntax::common::PiSig::*;

use super::{Axiom, Closure, Neutral, Val, ValInfo};
use crate::syntax::core::{CaseSplit, Variants};

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
            SplitOn(split, on) => {
                write!(f, "(case {} of {{ ", on)?;
                pretty_split(f, &split)?;
                f.write_str("})")
            }
            OrSplit(split, or) => {
                f.write_str("(cases {{ ")?;
                pretty_split(f, &split)?;
                write!(f, "}} or {})", or)
            }
            Fst(p) => write!(f, "({}.1)", p),
            Snd(p) => write!(f, "({}.2)", p),
            Proj(rec, field) => write!(f, "({}.{})", rec, field),
            Lift(levels, p) => write!(f, "(^[{:?}] {})", levels, p),
            Row(kind, variants, ext) => {
                write!(f, "{} {{", kind)?;
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
            Implicit(uid, mi) => write!(f, "{{{} {}}}", uid, mi),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Closure::*;
        match self {
            Plain(body) => body.fmt(f),
            Tree(split) => {
                for (label, closure) in split {
                    write!(f, "{} => {}; ", label, closure)?;
                }
                Ok(())
            }
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
                write!(f, "{}{} {{", kind, l)?;
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
                write!(f, "{} {{", kind)?;
                write_variants(f, variants, ":")?;
                f.write_str("}")
            }
            Val::Rec(fields) => {
                f.write_str("{|")?;
                write_variants(f, fields, " =")?;
                f.write_str("|}")
            }
            Val::Dt(Pi, _, param_ty, clos) => write!(f, "({} -> {})", param_ty, clos),
            Val::Dt(Sigma, _, param_ty, clos) => write!(f, "({} * {})", param_ty, clos),
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

fn pretty_split(f: &mut Formatter, split: &CaseSplit) -> Result<(), Error> {
    for (name, closure) in split {
        write!(f, "{}: \\ {}; ", name, closure)?;
    }
    Ok(())
}
