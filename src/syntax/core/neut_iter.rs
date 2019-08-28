use crate::syntax::core::{Closure, Neutral, Val};

pub trait TraverseNeutral: Sized {
    /// Map all [`Neutral`](self::Neutral) values in this expression.
    fn try_map_neutral<R>(self, f: &mut impl FnMut(Neutral) -> Result<Val, R>) -> Result<Self, R>;

    /// Fold all [`Neutral`](self::Neutral) values in this expression.
    fn try_fold_neutral<E, R>(
        self,
        init: R,
        f: impl Fn(R, Neutral) -> Result<R, E> + Copy,
    ) -> Result<R, E>;

    /// Traverse all [`Neutral`](self::Neutral) values the AST in a stateful manner.
    fn fold_neutral<R>(self, init: R, f: impl Fn(R, Neutral) -> R + Copy) -> R {
        self.try_fold_neutral(init, |r, v| Ok::<_, ()>(f(r, v)))
            .unwrap()
    }

    /// Traverse through the AST and change all [`Neutral`](self::Neutral) values.
    fn map_neutral(self, f: &mut impl FnMut(Neutral) -> Val) -> Self {
        let result: Result<_, ()> = self.try_map_neutral(&mut |neut| Ok(f(neut)));
        result.unwrap()
    }
}

impl TraverseNeutral for Val {
    fn try_map_neutral<R>(self, f: &mut impl FnMut(Neutral) -> Result<Val, R>) -> Result<Self, R> {
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
            Val::Dt(kind, param_plicit, param_type, closure) => Ok(Self::dependent_type(
                kind,
                param_plicit,
                param_type.try_map_neutral(f)?,
                closure.try_map_neutral(f)?,
            )),
            Val::Cons(name, a) => Ok(Self::cons(name, a.try_map_neutral(f)?)),
            e => Ok(e),
        }
    }

    fn try_fold_neutral<E, R>(
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
            Val::Dt(_, _, param_ty, closure) => closure
                .try_fold_neutral(init, f)
                .and_then(|r| param_ty.try_fold_neutral(r, f)),
            Val::Cons(_, a) => a.try_fold_neutral(init, f),
            Val::Type(..) | Val::RowKind(..) => Ok(init),
        }
    }
}

impl TraverseNeutral for Closure {
    fn try_map_neutral<R>(self, f: &mut impl FnMut(Neutral) -> Result<Val, R>) -> Result<Self, R> {
        use Closure::*;
        match self {
            Plain(body) => body.try_map_neutral(f).map(Self::plain),
            Tree(split) => split
                .into_iter()
                .map(|(k, v)| v.try_map_neutral(f).map(|v| (k, v)))
                .collect::<Result<_, _>>()
                .map(Closure::Tree),
        }
    }

    fn try_fold_neutral<E, R>(
        self,
        init: R,
        f: impl Fn(R, Neutral) -> Result<R, E> + Copy,
    ) -> Result<R, E> {
        use Closure::*;
        match self {
            Plain(body) => body.try_fold_neutral(init, f),
            Tree(split) => split
                .into_iter()
                .try_fold(init, |init, (_, v)| v.try_fold_neutral(init, f)),
        }
    }
}
