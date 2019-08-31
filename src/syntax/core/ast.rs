use std::collections::BTreeMap;

use crate::syntax::common::{PiSig, Plicit, VarRec, DBI, GI, MI, UID};
use crate::syntax::level::Level;

use super::{RedEx, TraverseNeutral};

/// Row variants -- for both variant type and record type.
pub type Variants = BTreeMap<String, TVal>;

/// Record fields -- for record values.
pub type Fields = BTreeMap<String, Val>;

/// Case-split expression.
pub type CaseSplit = BTreeMap<String, Closure>;

/// Reduction functions.
impl Val {
    pub fn apply(self, arg: Val) -> Self {
        match self {
            Val::Lam(closure) => closure.instantiate(arg),
            Val::Neut(Neutral::OrSplit(split, ..)) => Closure::Tree(split).instantiate(arg),
            Val::Neut(Neutral::App(f, mut a)) => {
                a.push(arg);
                Val::app(*f, a)
            }
            Val::Neut(otherwise) => Val::app(otherwise, vec![arg]),
            e => panic!("Cannot apply on `{}`.", e),
        }
    }

    pub fn first(self) -> Self {
        match self {
            Val::Pair(a, _) => *a,
            Val::Neut(otherwise) => Val::fst(otherwise),
            e => panic!("Cannot project on `{}`.", e),
        }
    }

    pub fn second(self) -> Val {
        match self {
            Val::Pair(_, b) => *b,
            Val::Neut(otherwise) => Val::snd(otherwise),
            e => panic!("Cannot project on `{}`.", e),
        }
    }

    pub fn project(self, field: String) -> Val {
        match self {
            Val::Rec(mut fields) => fields
                .remove(&field)
                .expect(&format!("Missing essential field with name `{}`.", field)),
            Val::Neut(Neutral::Rec(mut fields, ..)) => fields
                .remove(&field)
                .expect(&format!("Missing essential field with name `{}`.", field)),
            Val::Neut(otherwise) => Val::proj(otherwise, field),
            e => panic!("Cannot project on `{}`.", e),
        }
    }

    /// Extension for records.
    pub fn rec_extend(self, ext: Self) -> Self {
        use Val::*;
        match (self, ext) {
            (Rec(mut fields), Rec(mut ext)) => {
                fields.append(&mut ext);
                Rec(fields)
            }
            (Rec(mut fields), Neut(Neutral::Rec(mut more, ext))) => {
                fields.append(&mut more);
                Rec(fields).rec_extend(Neut(*ext))
            }
            (Rec(fields), Neut(otherwise)) => Self::neutral_record(fields, otherwise),
            (a, b) => panic!("Cannot extend `{}` by `{}`.", a, b),
        }
    }

    /// Extension for case-splits.
    pub fn split_extend(self, ext: Self) -> Self {
        use {Closure::Tree, Val::*};
        match (self, ext) {
            (Lam(Tree(mut split)), Lam(Tree(mut ext))) => {
                split.append(&mut ext);
                Lam(Tree(split))
            }
            (Lam(Tree(mut split)), Neut(Neutral::OrSplit(mut more, ext))) => {
                split.append(&mut more);
                Lam(Tree(split)).split_extend(Neut(*ext))
            }
            (Lam(Tree(split)), Neut(otherwise)) => Val::or_split(split, otherwise),
            (a, b) => panic!("Cannot extend `{}` by `{}`.", a, b),
        }
    }

    /// Extension for row-polymorphic types.
    pub fn row_extend(self, ext: Self) -> Self {
        use {Neutral::Row, Val::*, VarRec::*};
        match (self, ext) {
            (RowPoly(Record, mut fields), RowPoly(Record, mut ext)) => {
                fields.append(&mut ext);
                Self::record_type(fields)
            }
            (RowPoly(Record, mut fields), Neut(Row(Record, mut ext, more))) => {
                fields.append(&mut ext);
                Self::record_type(fields).row_extend(Neut(*more))
            }
            (RowPoly(Variant, mut variants), RowPoly(Variant, mut ext)) => {
                variants.append(&mut ext);
                Self::variant_type(variants)
            }
            (RowPoly(Variant, mut variants), Neut(Row(Variant, mut ext, more))) => {
                variants.append(&mut ext);
                Self::variant_type(variants).row_extend(Neut(*more))
            }
            (RowPoly(kind, mut variants), RowPoly(_, mut ext)) => {
                eprintln!("Warning: incorrect row extension!");
                variants.append(&mut ext);
                RowPoly(kind, variants)
            }
            (RowPoly(kind, mut variants), Neut(Row(_, mut ext, more))) => {
                eprintln!("Warning: incorrect row extension!");
                variants.append(&mut ext);
                RowPoly(kind, variants).row_extend(Neut(*more))
            }
            (RowPoly(kind, variants), Neut(otherwise)) => {
                if variants.is_empty() {
                    Neut(otherwise)
                } else {
                    Self::neutral_row_type(kind, variants, otherwise)
                }
            }
            (a, b) => panic!("Cannot extend `{}` by `{}`.", a, b),
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

    pub fn generated_to_var(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(&mut |a| match a {
            Postulated(..) | Unimplemented(..) | Implicit(..) => Axi(a),
            Generated(_, dbi) => Var(dbi),
        })
    }

    pub fn unimplemented_to_glob(self) -> Self {
        use {Axiom::*, Neutral::*};
        self.map_axiom(&mut |a| match a {
            Postulated(..) | Generated(..) | Implicit(..) => Axi(a),
            Unimplemented(_, dbi) => Ref(dbi),
        })
    }

    pub fn map_axiom(self, f: &mut impl FnMut(Axiom) -> Neutral) -> Self {
        self.map_neutral(&mut |neut| Val::Neut(neut.map_axiom(f)))
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
    /// Projecting a named element of a record.
    Proj(Box<Self>, String),
    /// Row-polymorphic types.
    Row(VarRec, Variants, Box<Self>),
    /// Record literal, with extension.
    Rec(Fields, Box<Self>),
    /// Splitting on a neutral term.
    SplitOn(CaseSplit, Box<Self>),
    /// Splitting with unknown branches.
    OrSplit(CaseSplit, Box<Self>),
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
    /// Implicit parameters during type-checking
    Implicit(UID),
}

impl Axiom {
    pub fn unique_id(&self) -> UID {
        use Axiom::*;
        match self {
            Postulated(uid) | Generated(uid, ..) | Unimplemented(uid, ..) | Implicit(uid, ..) => {
                *uid
            }
        }
    }
}

impl Neutral {
    pub fn map_axiom(self, f: &mut impl FnMut(Axiom) -> Neutral) -> Self {
        use Neutral::*;
        let mapper = &mut |n: Neutral| Val::Neut(n.map_axiom(f));
        let map_val = |(k, v): (String, Val)| (k, v.map_neutral(mapper));
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
            Proj(p, s) => Proj(Box::new(p.map_axiom(f)), s),
            Var(n) => Var(n),
            Ref(n) => Ref(n),
            Meta(n) => Meta(n),
            Lift(levels, expr) => Lift(levels, Box::new(expr.map_axiom(f))),
            Row(kind, variants, ext) => {
                let variants = variants.into_iter().map(map_val).collect();
                Row(kind, variants, Box::new(ext.map_axiom(f)))
            }
            Rec(fields, ext) => {
                let fields = fields.into_iter().map(map_val).collect();
                Rec(fields, Box::new(ext.map_axiom(f)))
            }
            SplitOn(split, obj) => SplitOn(
                Self::map_axiom_split(mapper, split),
                Box::new(obj.map_axiom(f)),
            ),
            OrSplit(split, obj) => OrSplit(
                Self::map_axiom_split(mapper, split),
                Box::new(obj.map_axiom(f)),
            ),
        }
    }

    fn map_axiom_split(mapper: &mut impl FnMut(Neutral) -> Val, split: CaseSplit) -> CaseSplit {
        split
            .into_iter()
            .map(|(k, v)| (k, v.map_neutral(mapper)))
            .collect()
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
    Dt(PiSig, Plicit, Box<Self>, Closure),
    /// Row-polymorphic type literal.
    RowPoly(VarRec, Variants),
    /// Row kind literals -- subtype of `Type`.
    RowKind(Level, VarRec, Vec<String>),
    /// Constructor invocation.
    Cons(String, Box<Self>),
    /// Record literal, without extension.
    Rec(Fields),
    /// Sigma instance.
    Pair(Box<Self>, Box<Self>),
    /// Neutral value means irreducible but not canonical values.
    Neut(Neutral),
}

impl Default for Val {
    fn default() -> Self {
        Self::fresh_axiom()
    }
}

/// A closure with parameter type explicitly specified.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Closure {
    Plain(Box<Val>),
    Tree(CaseSplit),
}

impl Default for Closure {
    fn default() -> Self {
        Closure::Tree(Default::default())
    }
}

impl Closure {
    pub fn instantiate(self, arg: Val) -> Val {
        match self {
            Closure::Plain(body) => body.reduce_with_dbi(arg, Default::default()),
            Closure::Tree(mut split) => match arg {
                Val::Cons(label, arg) => match split.remove(&label) {
                    Some(body) => body.instantiate(*arg),
                    // We can actually replace this pattern matching with `.expect`,
                    // but here we don't want to format the error message when things go correct.
                    None => panic!("Cannot find clause for label `{}`.", label),
                },
                Val::Neut(neutral) => Val::split_on(split, neutral),
                a => panic!("Cannot split on `{}`.", a),
            },
        }
    }

    pub fn instantiate_cloned(&self, arg: Val) -> Val {
        match self {
            Closure::Plain(body) => body.clone().reduce_with_dbi(arg, Default::default()),
            Closure::Tree(split) => match arg {
                Val::Cons(label, arg) => match split.get(&label) {
                    Some(body) => body.instantiate_cloned(*arg),
                    None => panic!("Cannot find clause for label `{}`.", label),
                },
                Val::Neut(neutral) => Val::split_on(split.clone(), neutral),
                a => panic!("Cannot split on `{}`.", a),
            },
        }
    }

    pub fn instantiate_borrow(&self, arg: &Val) -> Val {
        match self {
            Closure::Plain(body) => body.clone().reduce_with_dbi_borrow(arg, Default::default()),
            Closure::Tree(split) => match arg {
                Val::Cons(label, arg) => match split.get(label) {
                    Some(body) => body.instantiate_borrow(arg),
                    None => panic!("Cannot find clause for label `{}`.", label),
                },
                Val::Neut(neutral) => Val::split_on(split.clone(), neutral.clone()),
                a => panic!("Cannot split on `{}`.", a),
            },
        }
    }
}
