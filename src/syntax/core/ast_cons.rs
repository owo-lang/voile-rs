use crate::syntax::common::*;
use crate::syntax::core::{Axiom, CaseSplit, Closure, Fields, Neutral, TVal, Val, Variants};

/// Constructors and traversal functions.
impl Val {
    pub fn is_type(&self) -> bool {
        use Val::*;
        match self {
            Type(..) | Dt(..) | RowPoly(..) | RowKind(..) | Neut(Neutral::Row(..)) => true,
            // In case it's neutral, we use `is_universe` on its type.
            // In case it's a meta, we're supposed to solve it.
            Lam(..) | Cons(..) | Rec(..) | Pair(..) | Neut(..) => false,
        }
    }

    pub fn is_universe(&self) -> bool {
        match self {
            Val::Type(..) | Val::RowKind(..) => true,
            _ => false,
        }
    }

    pub fn pair(first: Self, second: Self) -> Self {
        Val::Pair(Box::new(first), Box::new(second))
    }

    pub fn cons(name: String, param: Self) -> Self {
        Val::Cons(name, Box::new(param))
    }

    pub fn lift(levels: u32, expr: Neutral) -> Self {
        Val::Neut(Neutral::Lift(levels, Box::new(expr)))
    }

    pub fn meta(index: MI) -> Self {
        Val::Neut(Neutral::Meta(index))
    }

    pub fn var(index: DBI) -> Self {
        Val::Neut(Neutral::Var(index))
    }

    pub fn closure_lam(body: Self) -> Self {
        Val::Lam(Closure::plain(body))
    }

    pub fn glob(index: GI) -> Self {
        Val::Neut(Neutral::Ref(index))
    }

    pub fn split_on(split: CaseSplit, on: Neutral) -> Self {
        Val::Neut(Neutral::SplitOn(split, Box::new(on)))
    }

    pub fn or_split(split: CaseSplit, or: Neutral) -> Self {
        Val::Neut(Neutral::OrSplit(split, Box::new(or)))
    }

    pub fn fresh_axiom() -> Self {
        Self::postulate(unsafe { next_uid() })
    }

    pub(crate) fn postulate(uid: UID) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Postulated(uid)))
    }

    pub fn fresh_implicit(mi: MI) -> Self {
        let axiom = Axiom::Implicit(unsafe { next_uid() }, mi);
        Val::Neut(Neutral::Axi(axiom))
    }

    pub fn fresh_unimplemented(index: GI) -> Self {
        let axiom = Axiom::Unimplemented(unsafe { next_uid() }, index);
        Val::Neut(Neutral::Axi(axiom))
    }

    pub fn app(function: Neutral, args: Vec<Self>) -> Self {
        Val::Neut(Neutral::App(Box::new(function), args))
    }

    pub fn fst(pair: Neutral) -> Self {
        Val::Neut(Neutral::Fst(Box::new(pair)))
    }

    pub fn snd(pair: Neutral) -> Self {
        Val::Neut(Neutral::Snd(Box::new(pair)))
    }

    pub fn proj(record: Neutral, field: String) -> Self {
        Val::Neut(Neutral::Proj(Box::new(record), field))
    }

    pub fn closure_dependent_type(
        kind: PiSig,
        param_plicit: Plicit,
        param_type: TVal,
        body: TVal,
    ) -> TVal {
        Self::dependent_type(kind, param_plicit, param_type, Closure::plain(body))
    }

    pub fn dependent_type(
        kind: PiSig,
        param_plicit: Plicit,
        param_type: TVal,
        closure: Closure,
    ) -> TVal {
        Val::Dt(kind, param_plicit, Box::new(param_type), closure)
    }

    pub fn variant_type(variants: Variants) -> TVal {
        Val::RowPoly(VarRec::Variant, variants)
    }

    pub fn record_type(fields: Variants) -> TVal {
        Val::RowPoly(VarRec::Record, fields)
    }

    pub fn neutral_row_type(kind: VarRec, variants: Variants, ext: Neutral) -> TVal {
        Val::Neut(Neutral::Row(kind, variants, Box::new(ext)))
    }

    pub fn neutral_record(fields: Fields, ext: Neutral) -> Self {
        Val::Neut(Neutral::Rec(fields, Box::new(ext)))
    }

    pub fn neutral_variant_type(variants: Variants, ext: Neutral) -> TVal {
        Self::neutral_row_type(VarRec::Variant, variants, ext)
    }

    pub fn neutral_record_type(fields: Variants, ext: Neutral) -> TVal {
        Self::neutral_row_type(VarRec::Record, fields, ext)
    }

    pub fn pi(param_plicit: Plicit, param_type: TVal, body: Closure) -> TVal {
        Self::dependent_type(PiSig::Pi, param_plicit, param_type, body)
    }

    pub fn sig(param_type: TVal, body: Closure) -> TVal {
        Self::dependent_type(PiSig::Sigma, Plicit::Ex, param_type, body)
    }

    pub fn into_neutral(self) -> Result<Neutral, Self> {
        match self {
            Val::Neut(n) => Ok(n),
            e => Err(e),
        }
    }
}

impl Closure {
    pub fn plain(body: Val) -> Self {
        Closure::Plain(Box::new(body))
    }
}
