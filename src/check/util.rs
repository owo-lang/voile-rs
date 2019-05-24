use std::collections::btree_map::BTreeMap;

use crate::check::monad::TCS;
use crate::syntax::abs::Abs;
use crate::syntax::common::{DtKind, Level, SyntaxInfo, DBI};
use crate::syntax::core::{Closure, Term, TermInfo};

macro_rules! compile {
    () => {impl Fn(TCS, Abs) -> Out};
}
type Out = (TermInfo, TCS);
type Abs2 = (Box<Abs>, Box<Abs>);

fn unsafe_compile_fst(tcs: TCS, info: SyntaxInfo, p: Box<Abs>, recursion: compile!()) -> Out {
    let (p, tcs) = recursion(tcs, *p);
    (p.ast.first().into_info(info), tcs)
}

fn unsafe_compile_snd(tcs: TCS, info: SyntaxInfo, p: Box<Abs>, recursion: compile!()) -> Out {
    let (p, tcs) = recursion(tcs, *p);
    (p.ast.second().into_info(info), tcs)
}

fn unsafe_compile_pair(tcs: TCS, info: SyntaxInfo, (a, b): Abs2, recursion: compile!()) -> Out {
    let (a, tcs) = recursion(tcs, *a);
    let (b, tcs) = recursion(tcs, *b);
    (Term::pair(a.ast, b.ast).into_info(info), tcs)
}

fn unsafe_compile_dt(
    tcs: TCS,
    info: SyntaxInfo,
    kind: DtKind,
    (param_ty, ret_ty): Abs2,
    recursion: compile!(),
) -> Out {
    let (param_ty, tcs) = recursion(tcs, *param_ty);
    let (ret_ty, tcs) = recursion(tcs, *ret_ty);
    let closure = Closure::new(param_ty.ast, ret_ty.ast);
    let term = Term::Dt(kind, closure);
    (term.into_info(info), tcs)
}

/// This function will produce **dbi**-based variable references.
///
/// Ensure `abs` is well-typed before invoking this,
/// otherwise this function may panic or produce ill-typed core term.
fn unsafe_compile(tcs: TCS, abs: Abs) -> Out {
    match abs {
        Abs::Type(info, level) => unsafe_compile_type(tcs, info, level),
        Abs::Bot(info) => unsafe_compile_bot(tcs, info),
        Abs::Local(info, _, dbi) => (Term::var(dbi).into_info(info), tcs),
        Abs::Var(info, dbi) => unsafe_compile_glob(tcs, info, dbi),
        Abs::Meta(info) => panic!("Cannot compile meta variable at {}.", info),
        Abs::Cons(_) => unimplemented!(),
        Abs::ConsType(_) => unimplemented!(),
        Abs::App(info, f, a) => {
            let (f, tcs) = unsafe_compile(tcs, *f);
            let (a, tcs) = unsafe_compile(tcs, *a);
            (f.ast.apply(a.ast).into_info(info), tcs)
        }
        Abs::Dt(info, kind, _, param_ty, ret_ty) => {
            unsafe_compile_dt(tcs, info, kind, (param_ty, ret_ty), unsafe_compile)
        }
        Abs::Pair(info, a, b) => unsafe_compile_pair(tcs, info, (a, b), unsafe_compile),
        Abs::Fst(info, p) => unsafe_compile_fst(tcs, info, p, unsafe_compile),
        Abs::Snd(info, p) => unsafe_compile_snd(tcs, info, p, unsafe_compile),
        Abs::Sum(_, _) => unimplemented!(),
        Abs::Lam(info, _, _, body) => {
            let (body, tcs) = unsafe_compile(tcs, *body);
            (body.ast.into_info(info), tcs)
        }
    }
}

/// This function will produce **uid**-based variable references.
///
/// Ensure `abs` is well-typed before invoking this,
/// otherwise this function may panic or produce ill-typed core term.
fn unsafe_evaluate(tcs: TCS, abs: Abs) -> Out {
    match abs {
        Abs::Type(info, level) => unsafe_compile_type(tcs, info, level),
        Abs::Bot(info) => unsafe_compile_bot(tcs, info),
        Abs::Local(info, _, dbi) => (tcs.local_val(dbi).ast.into_info(info), tcs),
        Abs::Var(info, dbi) => unsafe_compile_glob(tcs, info, dbi),
        Abs::Meta(info) => panic!("Cannot compile meta variable at {}.", info),
        Abs::Cons(info) => unimplemented!(),
        Abs::ConsType(_) => unimplemented!(),
        Abs::App(info, f, a) => {
            // Yes, functions should be `compiled` instead of `evaluated`!
            let (f, tcs) = unsafe_compile(tcs, *f);
            let (a, tcs) = unsafe_evaluate(tcs, *a);
            (f.ast.apply(a.ast).into_info(info), tcs)
        }
        Abs::Dt(info, kind, _, param_ty, ret_ty) => {
            unsafe_compile_dt(tcs, info, kind, (param_ty, ret_ty), unsafe_evaluate)
        }
        Abs::Pair(info, a, b) => unsafe_compile_pair(tcs, info, (a, b), unsafe_evaluate),
        Abs::Fst(info, p) => unsafe_compile_fst(tcs, info, p, unsafe_evaluate),
        Abs::Snd(info, p) => unsafe_compile_snd(tcs, info, p, unsafe_evaluate),
        Abs::Sum(_, _) => unimplemented!(),
        Abs::Lam(info, _, _, body) => {
            let (body, tcs) = unsafe_evaluate(tcs, *body);
            (body.ast.into_info(info), tcs)
        }
    }
}

fn unsafe_compile_type(tcs: TCS, info: SyntaxInfo, level: Level) -> Out {
    (Term::Type(level).into_info(info), tcs)
}

fn unsafe_compile_bot(tcs: TCS, info: SyntaxInfo) -> Out {
    (Term::Bot(0).into_info(info), tcs)
}

fn unsafe_compile_glob(tcs: TCS, info: SyntaxInfo, dbi: DBI) -> Out {
    (tcs.glob_val(dbi).ast.into_info(info), tcs)
}

pub fn compile_cons_type(info: &SyntaxInfo, ret_ty: &Closure) -> TermInfo {
    let mut variant = BTreeMap::default();
    variant.insert(info.text.clone(), Term::var(0));
    let lam = Term::lam(*ret_ty.param_type.clone(), Term::Sum(variant));
    lam.into_info(info.clone())
}

/// So you can do some functional programming based on method call chains.
impl TCS {
    #[inline]
    pub fn unsafe_compile(self, abs: Abs) -> (TermInfo, Self) {
        unsafe_compile(self, abs)
    }

    #[inline]
    pub fn unsafe_evaluate(self, abs: Abs) -> (TermInfo, Self) {
        unsafe_evaluate(self, abs)
    }
}
