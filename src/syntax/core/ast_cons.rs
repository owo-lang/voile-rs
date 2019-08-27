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

    pub fn fresh_axiom() -> Self {
        Self::postulate(unsafe { next_uid() })
    }

    pub(crate) fn postulate(uid: UID) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Postulated(uid)))
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

    pub fn closure_dependent_type(kind: PiSig, param_type: TVal, body: TVal) -> TVal {
        Self::dependent_type(kind, param_type, Closure::plain(body))
    }

    pub fn dependent_type(kind: PiSig, param_type: TVal, closure: Closure) -> TVal {
        Val::Dt(kind, Box::new(param_type), closure)
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

    pub fn pi(param_type: TVal, body: Closure) -> TVal {
        Self::dependent_type(PiSig::Pi, param_type, body)
    }

    pub fn sig(param_type: TVal, body: Closure) -> TVal {
        Self::dependent_type(PiSig::Sigma, param_type, body)
    }

    pub fn into_neutral(self) -> Result<Neutral, Self> {
        match self {
            Val::Neut(n) => Ok(n),
            e => Err(e),
        }
    }

    /// Traverse through the AST and change all [`Neutral`](self::Neutral) values.
    pub fn map_neutral(self, f: &mut impl FnMut(Neutral) -> Self) -> Self {
        let result: Result<_, ()> = self.try_map_neutral(&mut |neut| Ok(f(neut)));
        result.unwrap()
    }

    /// Traverse through the AST and change all [`Neutral`](self::Neutral) values.
    pub fn try_map_neutral<R>(
        self,
        f: &mut impl FnMut(Neutral) -> Result<Val, R>,
    ) -> Result<Self, R> {
        match self {
            Val::Neut(n) => f(n),
            Val::Pair(a, b) => Ok(Self::pair(a.try_map_neutral(f)?, b.try_map_neutral(f)?)),
            Val::RowPoly(kind, v) => v
                .into_iter()
                .map(|(k, v)| v.try_map_neutral(f).map(|v| (k, v)))
                .collect::<Result<_, _>>()
                .map(|vs| Val::RowPoly(kind, vs)),
            Val::Rec(fields) => fields
                .into_iter()
                .map(|(k, v)| v.try_map_neutral(f).map(|v| (k, v)))
                .collect::<Result<_, _>>()
                .map(Val::Rec),
            Val::Lam(closure) => closure.try_map_neutral(f).map(Self::Lam),
            Val::Dt(kind, param_type, closure) => Ok(Self::dependent_type(
                kind,
                param_type.try_map_neutral(f)?,
                closure.try_map_neutral(f)?,
            )),
            Val::Cons(name, a) => Ok(Self::cons(name, a.try_map_neutral(f)?)),
            e => Ok(e),
        }
    }

    pub fn map_axiom(self, f: &mut impl FnMut(Axiom) -> Neutral) -> Self {
        self.map_neutral(&mut |neut| Val::Neut(neut.map_axiom(f)))
    }

    pub fn generated_to_var(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(&mut |a| match a {
            Postulated(..) | Unimplemented(..) => Axi(a),
            Generated(_, dbi) => Var(dbi),
        })
    }

    pub fn unimplemented_to_glob(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(&mut |a| match a {
            Postulated(..) | Generated(..) => Axi(a),
            Unimplemented(_, dbi) => Ref(dbi),
        })
    }

    /// Traverse through the AST in a stateful manner.
    pub fn fold_neutral<R>(self, init: R, f: impl Fn(R, Neutral) -> R + Copy) -> R {
        self.try_fold_neutral(init, |r, v| Ok::<_, ()>(f(r, v)))
            .unwrap()
    }

    /// Traverse through the AST with possible error.
    pub fn try_fold_neutral<E, R>(
        self,
        init: R,
        f: impl Fn(R, Neutral) -> Result<R, E> + Copy,
    ) -> Result<R, E> {
        match self {
            Val::Neut(n) => f(init, n),
            Val::Pair(a, b) => a
                .try_fold_neutral(init, f)
                .and_then(|r| b.try_fold_neutral(r, f)),
            Val::RowPoly(_, v) => v
                .into_iter()
                .try_fold(init, |a, (_, v)| v.try_fold_neutral(a, f)),
            Val::Rec(fields) => fields
                .into_iter()
                .try_fold(init, |a, (_, v)| v.try_fold_neutral(a, f)),
            Val::Lam(closure) => closure.try_fold_neutral(init, f),
            Val::Dt(_, param_ty, closure) => closure
                .try_fold_neutral(init, f)
                .and_then(|r| param_ty.try_fold_neutral(r, f)),
            Val::Cons(_, a) => a.try_fold_neutral(init, f),
            Val::Type(..) | Val::RowKind(..) => Ok(init),
        }
    }
}
