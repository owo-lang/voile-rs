use voile_util::level::Level;
use voile_util::loc::*;
use voile_util::meta::MI;
use voile_util::tags::*;
use voile_util::uid::*;

use crate::syntax::common::*;

pub type LabAbs = Labelled<Abs>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Abs {
    Type(Loc, Level),
    /// Local variable
    Var(Ident, UID, DBI),
    /// Global variable
    Ref(Ident, GI),
    /// Meta variable
    Meta(Ident, MI),
    /// Lift an expression many times
    Lift(Loc, u32, Box<Self>),
    /// Constructor call
    Cons(Ident),
    /// Record projection
    Proj(Loc, Box<Self>, Ident),
    /// Apply or Pipeline in surface
    App(Loc, Box<Self>, Plicit, Box<Self>),
    /// Dependent Type, `(a -> b -> c)` as `Dt(_, DtKind::Pi, _, _, a, Dt(_, DtKind::Pi, _, _, b, c))`
    Dt(Loc, PiSig, UID, Plicit, Box<Self>, Box<Self>),
    /// The first `Loc` is the syntax info of this whole lambda,
    /// while the second is about its parameter
    Lam(Loc, Ident, UID, Box<Self>),
    Pair(Loc, Box<Self>, Box<Self>),
    Fst(Loc, Box<Self>),
    Snd(Loc, Box<Self>),
    /// Row-polymorphic types, corresponds to [RowPoly](crate::syntax::surf::Expr::RowPoly)
    RowPoly(Loc, VarRec, Vec<LabAbs>, Option<Box<Self>>),
    /// Record literals
    Rec(Loc, Vec<LabAbs>, Option<Box<Self>>),
    /// Empty type eliminator,
    Whatever(Loc),
    /// Case-split expressions.
    CaseOr(Ident, Ident, UID, Box<Self>, Box<Self>),
    /// Row-polymorphic kinds, corresponds to [RowKind](crate::syntax::surf::Expr::RowKind)
    RowKind(Loc, VarRec, Vec<Ident>),
}

impl ToLoc for Abs {
    fn loc(&self) -> Loc {
        match self {
            Abs::Type(info, ..)
            | Abs::App(info, ..)
            | Abs::Dt(info, ..)
            | Abs::Pair(info, ..)
            | Abs::Fst(info, ..)
            | Abs::Snd(info, ..)
            | Abs::RowPoly(info, ..)
            | Abs::Rec(info, ..)
            | Abs::Proj(info, ..)
            | Abs::RowKind(info, ..)
            | Abs::Lift(info, ..)
            | Abs::Whatever(info)
            | Abs::Lam(info, ..) => (*info).clone(),
            Abs::CaseOr(ident, _, _, _, last) => merge_info(ident, &**last),
            Abs::Var(ident, ..) | Abs::Ref(ident, ..) | Abs::Meta(ident, ..) | Abs::Cons(ident) => {
                ident.loc.clone()
            }
        }
    }
}

impl Abs {
    pub fn dependent_type(
        info: Loc,
        kind: PiSig,
        name: UID,
        plicit: Plicit,
        a: Self,
        b: Self,
    ) -> Self {
        Abs::Dt(info, kind, name, plicit, Box::new(a), Box::new(b))
    }

    pub fn row_polymorphic_type(
        info: Loc,
        kind: VarRec,
        labels: Vec<LabAbs>,
        rest: Option<Self>,
    ) -> Self {
        Abs::RowPoly(info, kind, labels, rest.map(Box::new))
    }

    pub fn record(info: Loc, fields: Vec<LabAbs>, rest: Option<Self>) -> Self {
        Abs::Rec(info, fields, rest.map(Box::new))
    }

    pub fn app(info: Loc, function: Self, plicit: Plicit, argument: Self) -> Self {
        Abs::App(info, Box::new(function), plicit, Box::new(argument))
    }

    pub fn proj(info: Loc, record: Self, field: Ident) -> Self {
        Abs::Proj(info, Box::new(record), field)
    }

    pub fn fst(info: Loc, of: Self) -> Self {
        Abs::Fst(info, Box::new(of))
    }

    pub fn snd(info: Loc, of: Self) -> Self {
        Abs::Snd(info, Box::new(of))
    }

    pub fn lam(whole_info: Loc, param: Ident, name: UID, body: Self) -> Self {
        Abs::Lam(whole_info, param, name, Box::new(body))
    }

    pub fn pair(info: Loc, first: Self, second: Self) -> Self {
        Abs::Pair(info, Box::new(first), Box::new(second))
    }

    pub fn case_or(label: Ident, binding: Ident, uid: UID, clause: Self, or: Self) -> Self {
        Abs::CaseOr(label, binding, uid, Box::new(clause), Box::new(or))
    }

    pub fn lift(info: Loc, lift_count: u32, expr: Self) -> Self {
        Abs::Lift(info, lift_count, Box::new(expr))
    }

    pub fn pi(info: Loc, name: UID, plicit: Plicit, input: Self, output: Self) -> Self {
        Self::dependent_type(info, PiSig::Pi, name, plicit, input, output)
    }

    pub fn sig(info: Loc, name: UID, plicit: Plicit, first: Self, second: Self) -> Self {
        Self::dependent_type(info, PiSig::Sigma, name, plicit, first, second)
    }
}

/// Type signature and body implementation,
/// with abstract syntax.
#[derive(Debug, Clone)]
pub enum AbsDecl {
    /// Signature.
    Sign(Abs, GI),
    /// Function body without a signature.
    Decl(Abs),
    /// Function body with a signature.
    Impl(Abs, GI),
}

impl ToLoc for AbsDecl {
    fn loc(&self) -> Loc {
        use AbsDecl::*;
        match self {
            Sign(abs, ..) | Decl(abs) | Impl(abs, ..) => abs.loc(),
        }
    }
}
