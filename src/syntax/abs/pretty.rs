use std::fmt::{Display, Error, Formatter};

use PiSig::*;

use crate::syntax::common::{PiSig, Plicit};

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
            Abs::App(_, a, _, b) => write!(f, "({} {})", a, b),
            Abs::Dt(_, Pi, name, Plicit::Ex, param, ret) => {
                write!(f, "({{{:?}}} : {}) -> {}", name, param, ret)
            }
            Abs::Dt(_, Pi, name, Plicit::Im(_), param, ret) => {
                write!(f, "({{{:?}}} : {}) -> {}", name, param, ret)
            }
            Abs::Dt(_, Sigma, name, _, fst, snd) => write!(f, "(<{:?}> : {}) * {}", name, fst, snd),
            Abs::Lam(_, param, name, body) => write!(f, "(\\{}[{:?}]. {})", param.text, name, body),
            Abs::Pair(_, a, b) => write!(f, "({}, {})", a, b),
            Abs::Fst(_, p) => write!(f, "({}.1)", p),
            Abs::Snd(_, p) => write!(f, "({}.2)", p),
            Abs::Proj(_, rec, field) => write!(f, "({}.{})", rec, field.text),
            Abs::Whatever(..) => f.write_str("whatever"),
            Abs::CaseOr(label, binding, _, body, or) => write!(
                f,
                "(case {} {}: {} or {})",
                label.text, binding.text, body, or
            ),
            Abs::RowKind(_, kind, labels) => {
                write!(f, "{} [ ", kind)?;
                for ident in labels {
                    write!(f, "{} ", ident.text)?;
                }
                write!(f, "]")
            }
            Abs::RowPoly(_, kind, labels, rest) => {
                write!(f, "{} {{ ", kind)?;
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
