use std::fmt::{Display, Error, Formatter};

use PiSig::*;
use VarRec::*;

use crate::syntax::common::{PiSig, VarRec};

use super::{Abs, AbsDecl, LabAbs};

type MonadFmt = Result<(), Error>;

impl Display for Abs {
    fn fmt(&self, f: &mut Formatter) -> MonadFmt {
        match self {
            Abs::Type(_, level) => write!(f, "set{}", level),
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
            Abs::RowKind(_, kind, labels) => {
                let prefix = match kind {
                    Variant => "Sum",
                    Record => "Rec",
                };
                write!(f, "{} [ ", prefix)?;
                for ident in labels {
                    write!(f, "{} ", ident.text)?;
                }
                write!(f, "]")
            }
            Abs::RowPoly(_, kind, labels, rest) => {
                let prefix = match kind {
                    Variant => "Sum",
                    Record => "Rec",
                };
                write!(f, "{} {{ ", prefix)?;
                pretty_labels(f, labels, ":")?;
                match rest {
                    Some(rest) => write!(f, "... = {} }}", rest),
                    None => write!(f, "}}"),
                }
            }
            Abs::Rec(_, fields, rest) => {
                f.write_str("{|")?;
                pretty_labels(f, fields, " =")?;
                match rest {
                    Some(rest) => write!(f, "... = {} |}}", rest),
                    None => f.write_str("|}"),
                }
            }
        }
    }
}

fn pretty_labels(f: &mut Formatter, labels: &[LabAbs], sep: &str) -> MonadFmt {
    for label in labels {
        writeln!(f, "{}{} {}; ", label.label.text, sep, label.expr)?;
    }
    Ok(())
}

impl Display for AbsDecl {
    fn fmt(&self, f: &mut Formatter) -> MonadFmt {
        match self {
            AbsDecl::Sign(abs, dbi) => write!(f, "[{}] {}", dbi, abs),
            AbsDecl::Decl(abs) => write!(f, "_ : {}", abs),
            AbsDecl::Impl(abs, ty_dbi) => write!(f, "{} : [{}]", abs, ty_dbi),
        }
    }
}
