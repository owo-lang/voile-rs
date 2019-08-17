use std::collections::BTreeMap;

use crate::syntax::common::{next_uid, PiSig, VarRec, DBI, GI, MI, UID};
use crate::syntax::level::Level;

use super::level::*;

pub type Variants = BTreeMap<String, TVal>;

/// Reducible expressions.
pub trait RedEx: Sized {
    /// This is primarily a private implementation-related API.
    /// Use at your own risk.
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val;

    /// When the argument is not likely to be used,
    /// prefer this over [`reduce_with_dbi`](reduce_with_dbi).
    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Val;
}

impl Val {
    /**
    $$
    \\newcommand{\\inst}[0]{\\texttt{inst}}
    \\newcommand{\\app}[0]{\\texttt{app}}
    \\begin{alignedat}{1}
    \\app(\\lambda C, o) &= \\inst(C, o) \\\\
    \\app([k], o) &= [k\ o]
    \\end{alignedat}
    $$
    For evaluation during beta-reduction.
    */
    pub fn apply(self, arg: Val) -> Val {
        match self {
            Val::Lam(closure) => closure.instantiate(arg),
            Val::Neut(Neutral::App(f, mut a)) => {
                a.push(arg);
                Val::app(*f, a)
            }
            Val::Neut(n) => Val::app(n, vec![arg]),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    pub fn apply_borrow(self, arg: &Val) -> Val {
        match self {
            Val::Lam(closure) => closure.instantiate_borrow(arg),
            Val::Neut(Neutral::App(f, mut a)) => {
                a.push(arg.clone());
                Val::app(*f, a)
            }
            Val::Neut(n) => Val::app(n, vec![arg.clone()]),
            e => panic!("Cannot apply on `{:?}`.", e),
        }
    }

    /**
    $$
    \\newcommand{\\first}[0]{\\texttt{first}}
    \\newcommand{\\second}[0]{\\texttt{second}}
    \\begin{alignedat}{1}
    \\first(n, m) &= n \\\\
    \\first([k]) &= [k\ .1] \\\\
    \\end{alignedat}
    $$
    For evaluation during beta-reduction.
    */
    pub fn first(self) -> Val {
        match self {
            Val::Pair(a, _) => *a,
            Val::Neut(n) => Val::fst(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    /**
    $$
    \\newcommand{\\first}[0]{\\texttt{first}}
    \\newcommand{\\second}[0]{\\texttt{second}}
    \\begin{alignedat}{1}
    \\second(n, m) &= m \\\\
    \\second([k]) &= [k\ .2]
    \\end{alignedat}
    $$
    For evaluation during beta-reduction.
    */
    pub fn second(self) -> Val {
        match self {
            Val::Pair(_, b) => *b,
            Val::Neut(n) => Val::snd(n),
            e => panic!("Cannot project on `{:?}`.", e),
        }
    }

    pub(crate) fn attach_dbi(self, dbi: DBI) -> Self {
        self.map_neutral(&mut |neut: Neutral| {
            Val::Neut(neut.map_axiom(&mut |a| {
                Neutral::Axi(match a {
                    Axiom::Postulated(uid) => Axiom::Generated(uid, dbi),
                    e => e,
                })
            }))
        })
    }
}

impl RedEx for Val {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        match self {
            Val::Pair(a, b) => Val::pair(
                a.reduce_with_dbi_borrow(&arg, dbi),
                b.reduce_with_dbi(arg, dbi),
            ),
            Val::Neut(neutral_value) => neutral_value.reduce_with_dbi(arg, dbi),
            Val::Lam(Closure { body }) => Val::lam(body.reduce_with_dbi(arg, dbi + 1)),
            Val::Dt(kind, param_type, Closure { body }) => Val::dependent_type(
                kind,
                param_type.reduce_with_dbi_borrow(&arg, dbi),
                body.reduce_with_dbi(arg, dbi + 1),
            ),
            Val::RowPoly(kind, variants) => Val::RowPoly(
                kind,
                variants
                    .into_iter()
                    .map(|(name, ty)| (name, ty.reduce_with_dbi_borrow(&arg, dbi)))
                    .collect(),
            ),
            Val::Cons(name, a) => Self::cons(name, a.reduce_with_dbi(arg, dbi)),
            Val::Type(n) => Val::Type(n),
        }
    }

    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Val {
        match self {
            Val::Pair(a, b) => Val::pair(
                a.reduce_with_dbi_borrow(arg, dbi),
                b.reduce_with_dbi_borrow(arg, dbi),
            ),
            Val::Neut(neutral_value) => neutral_value.reduce_with_dbi_borrow(arg, dbi),
            Val::Lam(Closure { body }) => Val::lam(body.reduce_with_dbi_borrow(arg, dbi + 1)),
            Val::Dt(kind, param_type, Closure { body }) => Val::dependent_type(
                kind,
                param_type.reduce_with_dbi_borrow(arg, dbi),
                body.reduce_with_dbi_borrow(arg, dbi + 1),
            ),
            Val::RowPoly(kind, variants) => Val::RowPoly(
                kind,
                variants
                    .into_iter()
                    .map(|(name, ty)| (name, ty.reduce_with_dbi_borrow(arg, dbi)))
                    .collect(),
            ),
            Val::Cons(name, a) => Self::cons(name, a.reduce_with_dbi_borrow(arg, dbi)),
            Val::Type(n) => Val::Type(n),
        }
    }
}

/// Irreducible because of the presence of generated value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Neutral {
    /// Local variable, referred by de-bruijn index.
    Var(DBI),
    /// Global variable, referred by index. Needed for recursive definitions.
    Ref(GI),
    /// Meta variable reference.
    Meta(MI),
    /// Lifting self to a higher/lower level.
    Lift(u32, Box<Self>),
    /// Postulated value, aka axioms.
    Axi(Axiom),
    /// Function application, with all arguments collected
    /// (so we have easy access to application arguments).<br/>
    /// This is convenient for meta resolution and termination check.
    ///
    /// The "arguments" is supposed to be non-empty.
    App(Box<Self>, Vec<Val>),
    /// Projecting the first element of a pair.
    Fst(Box<Self>),
    /// Projecting the second element of a pair.
    Snd(Box<Self>),
    /// Row-polymorphic types.
    Row(VarRec, Variants, Box<Self>),
}

/// Postulated value (or temporarily irreducible expressions), aka axioms.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Axiom {
    /// Functions without implementation.
    Postulated(UID),
    /// Lambda parameters during type-checking.
    /// (usually will be replaced with `Val::var` after the expression is type-checked).
    Generated(UID, DBI),
    /// Usages of definitions when they're not yet implemented.
    /// (usually will be replaced with `Val::glob` after implemented).
    Unimplemented(UID, GI),
}

impl Axiom {
    pub fn unique_id(&self) -> UID {
        use Axiom::*;
        match self {
            Postulated(uid) | Generated(uid, ..) | Unimplemented(uid, ..) => *uid,
        }
    }
}

impl Neutral {
    pub fn map_axiom(self, f: &mut impl FnMut(Axiom) -> Neutral) -> Self {
        use Neutral::*;
        match self {
            Axi(a) => f(a),
            App(fun, args) => App(
                Box::new(fun.map_axiom(f)),
                args.into_iter()
                    .map(|a| a.map_neutral(&mut |n| Val::Neut(n.map_axiom(f))))
                    .collect(),
            ),
            Fst(p) => Fst(Box::new(p.map_axiom(f))),
            Snd(p) => Snd(Box::new(p.map_axiom(f))),
            Var(n) => Var(n),
            Ref(n) => Ref(n),
            Meta(n) => Meta(n),
            Lift(levels, expr) => Lift(levels, Box::new(expr.map_axiom(f))),
            Row(kind, variants, ext) => {
                let mapper = &mut |n: Neutral| Val::Neut(n.map_axiom(f));
                let variants = variants
                    .into_iter()
                    .map(|(k, v)| (k, v.map_neutral(mapper)))
                    .collect();
                Row(kind, variants, Box::new(ext.map_axiom(f)))
            }
        }
    }
}

impl RedEx for Neutral {
    fn reduce_with_dbi(self, arg: Val, dbi: DBI) -> Val {
        use Neutral::*;
        match self {
            Var(n) if dbi == n => arg.attach_dbi(dbi),
            Var(n) => Val::var(n),
            Ref(n) => Val::glob(n),
            Meta(mi) => Val::meta(mi),
            Axi(a) => Val::Neut(Axi(a)),
            App(f, args) => args
                .into_iter()
                .fold(f.reduce_with_dbi_borrow(&arg, dbi), |f, a| {
                    // Do we need to `reduce` after `apply` again?
                    f.apply(a.reduce_with_dbi_borrow(&arg, dbi))
                }),
            Fst(pair) => pair.reduce_with_dbi(arg, dbi).first(),
            Snd(pair) => pair.reduce_with_dbi(arg, dbi).second(),
            Lift(levels, neut) => neut.reduce_with_dbi(arg, dbi).lift(levels),
            Row(..) => unimplemented!(), // TODO: calculation
        }
    }

    fn reduce_with_dbi_borrow(self, arg: &Val, dbi: DBI) -> Val {
        use Neutral::*;
        match self {
            Var(n) if dbi == n => arg.clone().attach_dbi(dbi),
            Var(n) => Val::var(n),
            Ref(n) => Val::glob(n),
            Meta(mi) => Val::meta(mi),
            Axi(a) => Val::Neut(Axi(a)),
            App(f, args) => args
                .into_iter()
                .fold(f.reduce_with_dbi_borrow(arg, dbi), |f, a| {
                    // Do we need to `reduce` after `apply` again?
                    f.apply(a.reduce_with_dbi_borrow(arg, dbi))
                }),
            Fst(pair) => pair.reduce_with_dbi_borrow(arg, dbi).first(),
            Snd(pair) => pair.reduce_with_dbi_borrow(arg, dbi).second(),
            Lift(levels, neut) => neut.reduce_with_dbi_borrow(arg, dbi).lift(levels),
            Row(..) => unimplemented!(), // TODO: calculation
        }
    }
}

/// Type values.
pub type TVal = Val;

/// Non-redex, canonical values.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Val {
    /// Type universe.
    Type(Level),
    /// Closure with parameter typed.
    /// For untyped closures, it can be represented as `Neut` directly.
    Lam(Closure),
    /// Pi-like types (dependent types), with parameter explicitly typed.
    Dt(PiSig, Box<Self>, Closure),
    /// Row-polymorphic type literal.
    RowPoly(VarRec, Variants),
    /// Constructor invocation.
    Cons(String, Box<Self>),
    /// Sigma instance.
    Pair(Box<Self>, Box<Self>),
    /// Neutral value means irreducible but not canonical values.
    Neut(Neutral),
}

impl Val {
    pub fn is_type(&self) -> bool {
        use Val::*;
        match self {
            Type(..) | Dt(..) | RowPoly(..) => true,
            // In case it's neutral, we use `is_universe` on its type.
            // In case it's a meta, we're supposed to solve it.
            Lam(..) | Cons(..) | Pair(..) | Neut(..) => false,
        }
    }

    pub fn is_universe(&self) -> bool {
        match self {
            Val::Type(_) => true,
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

    pub fn lam(body: Self) -> Self {
        Val::Lam(Closure::new(body))
    }

    pub fn glob(index: GI) -> Self {
        Val::Neut(Neutral::Ref(index))
    }

    pub fn fresh_axiom() -> Self {
        Self::postulate(unsafe { next_uid() })
    }

    pub(crate) fn postulate(uid: UID) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Postulated(uid)))
    }

    pub(crate) fn generate(uid: UID, dbi: DBI) -> Self {
        Val::Neut(Neutral::Axi(Axiom::Generated(uid, dbi)))
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

    pub fn dependent_type(kind: PiSig, param_type: TVal, body: TVal) -> TVal {
        Val::Dt(kind, Box::new(param_type), Closure::new(body))
    }

    pub fn variant_type(variants: Variants) -> TVal {
        Val::RowPoly(VarRec::Variant, variants)
    }

    pub fn record_type(fields: Variants) -> TVal {
        Val::RowPoly(VarRec::Record, fields)
    }

    pub fn pi(param_type: TVal, body: TVal) -> TVal {
        Self::dependent_type(PiSig::Pi, param_type, body)
    }

    pub fn sig(param_type: TVal, body: TVal) -> TVal {
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
        f: &mut impl FnMut(Neutral) -> Result<Self, R>,
    ) -> Result<Self, R> {
        match self {
            Val::Neut(n) => f(n),
            Val::Pair(a, b) => Ok(Self::pair(a.try_map_neutral(f)?, b.try_map_neutral(f)?)),
            Val::RowPoly(kind, v) => v
                .into_iter()
                .map(|(k, v)| v.try_map_neutral(f).map(|v| (k, v)))
                .collect::<Result<_, _>>()
                .map(|vs| Val::RowPoly(kind, vs)),
            Val::Lam(Closure { body }) => body.try_map_neutral(f).map(Self::lam),
            Val::Dt(kind, param_type, Closure { body }) => Ok(Self::dependent_type(
                kind,
                param_type.try_map_neutral(f)?,
                body.try_map_neutral(f)?,
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
    pub fn fold_neutral<R, F: Fn(R, Neutral) -> R + Copy>(self, init: R, f: F) -> R {
        self.try_fold_neutral(init, |r, v| Ok::<_, ()>(f(r, v)))
            .unwrap()
    }

    /// Traverse through the AST with possible error.
    pub fn try_fold_neutral<E, R, F: Fn(R, Neutral) -> Result<R, E> + Copy>(
        self,
        init: R,
        f: F,
    ) -> Result<R, E> {
        match self {
            Val::Neut(n) => f(init, n),
            Val::Pair(a, b) => a
                .try_fold_neutral(init, f)
                .and_then(|r| b.try_fold_neutral(r, f)),
            Val::RowPoly(_, v) => v
                .into_iter()
                .try_fold(init, |a, (_, v)| v.try_fold_neutral(a, f)),
            Val::Lam(Closure { body }) => body.try_fold_neutral(init, f),
            Val::Dt(_, param_ty, Closure { body }) => body
                .try_fold_neutral(init, f)
                .and_then(|r| param_ty.try_fold_neutral(r, f)),
            Val::Cons(_, a) => a.try_fold_neutral(init, f),
            Val::Type(_) => Ok(init),
        }
    }
}

impl Default for Val {
    fn default() -> Self {
        Self::fresh_axiom()
    }
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub body: Box<Val>,
}

impl Closure {
    pub fn new(body: Val) -> Self {
        Self {
            body: Box::new(body),
        }
    }

    pub fn instantiate(self, arg: Val) -> Val {
        self.body.reduce_with_dbi(arg, Default::default())
    }

    pub fn instantiate_cloned(&self, arg: Val) -> Val {
        self.body.clone().reduce_with_dbi(arg, Default::default())
    }

    pub fn instantiate_borrow(self, arg: &Val) -> Val {
        self.body.reduce_with_dbi_borrow(arg, Default::default())
    }

    pub fn instantiate_cloned_borrow(&self, arg: &Val) -> Val {
        self.body
            .clone()
            .reduce_with_dbi_borrow(arg, Default::default())
    }
}
