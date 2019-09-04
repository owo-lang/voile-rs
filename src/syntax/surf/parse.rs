use pest::Parser;
use pest_derive::Parser;

use voile_util::pest_util::end_of_rule;
use voile_util::vec1::Vec1;

use crate::syntax::common::{Ident, Plicit, SyntaxInfo, VarRec};
use crate::syntax::surf::LabExpr;
use voile_util::level::Level;

use super::ast::Param;
use super::{Decl, DeclKind, Expr};

#[derive(Parser)]
#[grammar = "syntax/surf/grammar.pest"]
/// The name stands for "Voile's Parser"
struct VoileParser;

tik_tok!();

define_parse_str!(parse_str, VoileParser, file, declarations, Vec<Decl>);
define_parse_str!(parse_str_expr, VoileParser, standalone_expr, expr, Expr);

macro_rules! expr_parser {
    ($name:ident,$smaller:ident,$cons:ident) => {
        fn $name(rules: Tok) -> Expr {
            let mut exprs: Vec<Expr> = Default::default();
            for smaller in rules.into_inner() {
                exprs.push($smaller(smaller));
            }
            let first = exprs.remove(0);
            if exprs.is_empty() {
                first
            } else {
                Expr::$cons(first, exprs)
            }
        }
    };
}

macro_rules! many_prefix_parser {
    ($name:ident, $prefix_ty:ident, $prefix:ident, $end:ident) => {
        fn $name(rules: Tok) -> (Vec<$prefix_ty>, Option<Expr>) {
            let mut prefixes = Vec::new();
            let mut end = None;
            for the_rule in rules.into_inner() {
                match the_rule.as_rule() {
                    Rule::$prefix => prefixes.push($prefix(the_rule)),
                    Rule::$end => end = Some($end(the_rule)),
                    e => panic!("Unexpected rule: {:?} with token {}.", e, the_rule.as_str()),
                }
            }
            (prefixes, end)
        }
    };
}

#[inline]
fn next_ident(inner: &mut Tik) -> Ident {
    next_rule!(inner, ident)
}

fn declarations(the_rule: Tok) -> Vec<Decl> {
    the_rule.into_inner().into_iter().map(declaration).collect()
}

fn rec_field(rules: Tok) -> LabExpr {
    labelled(rules)
}

fn labelled(rules: Tok) -> LabExpr {
    let mut inner: Tik = rules.into_inner();
    let label = next_ident(&mut inner);
    let expr = next_rule!(inner, expr);
    end_of_rule(&mut inner);
    LabExpr { expr, label }
}

fn row_rest(rules: Tok) -> Expr {
    let mut inner: Tik = rules.into_inner();
    let expr = next_rule!(inner, expr);
    end_of_rule(&mut inner);
    expr
}

many_prefix_parser!(row_polymorphic, LabExpr, labelled, row_rest);
many_prefix_parser!(record_literal, LabExpr, rec_field, row_rest);

fn record(rules: Tok) -> Expr {
    let info = SyntaxInfo::from(rules.as_span());
    let (fields, rest) = record_literal(rules);
    Expr::record(info, fields, rest)
}

fn variant_record(rules: Tok, kind: VarRec) -> Expr {
    let info = SyntaxInfo::from(rules.as_span());
    let mut inner: Tik = rules.into_inner();
    let (labels, rest) = next_rule!(inner, row_polymorphic);
    Expr::row_polymorphic_type(info, labels, kind, rest)
}

fn variant_record_kind(rules: Tok, kind: VarRec) -> Expr {
    let info = SyntaxInfo::from(rules.as_span());
    let rules = rules.into_inner().next().unwrap();
    let labels = rules.into_inner().into_iter().map(ident).collect();
    Expr::RowKind(info, kind, labels)
}

fn declaration(rules: Tok) -> Decl {
    let the_rule: Tok = rules.into_inner().next().unwrap();
    let kind = match the_rule.as_rule() {
        Rule::signature => DeclKind::Sign,
        Rule::implementation => DeclKind::Impl,
        _ => unreachable!(),
    };
    let mut inner: Tik = the_rule.into_inner();
    let name = next_ident(&mut inner);
    let body = next_rule!(inner, expr);
    end_of_rule(&mut inner);
    Decl { kind, name, body }
}

expr_parser!(dollar_expr, comma_expr, app);
expr_parser!(comma_expr, pipe_expr, tup);
expr_parser!(pipe_expr, lift_expr, pipe);
// expr_parser!(lift_expr, app_expr, lift); customized
expr_parser!(app_expr, primary_expr, app);

fn lift_expr(rules: Tok) -> Expr {
    let mut lift_count = 0u32;
    let syntax_info = From::from(rules.as_span());
    for smaller in rules.into_inner() {
        match smaller.as_rule() {
            Rule::lift_op => {
                lift_count += 1;
            }
            Rule::proj_expr => {
                let expr = proj_expr(smaller);
                return if lift_count == 0 {
                    expr
                } else {
                    Expr::lift(syntax_info, lift_count, expr)
                };
            }
            _ => unreachable!(),
        }
    }
    unreachable!()
}

fn proj_expr(rules: Tok) -> Expr {
    let mut projections = None;
    let mut inner = rules.into_inner();
    let projected = next_rule!(inner, app_expr);
    for projection in inner {
        assert_eq!(projection.as_rule(), Rule::proj_op);
        let ident = Ident {
            info: SyntaxInfo::from(projection.as_span()),
            text: projection.as_str()[1..].to_owned(),
        };
        match projections {
            None => projections = Some(Vec1::from(ident)),
            Some(mut some_projections) => {
                some_projections.push(ident);
                projections = Some(some_projections);
            }
        };
    }
    match projections {
        Some(projections) => Expr::proj(projected, projections),
        None => projected,
    }
}

fn expr(rules: Tok) -> Expr {
    let mut inner: Tik = rules.into_inner();
    let expr = next_rule!(inner, sig_expr);
    end_of_rule(&mut inner);
    expr
}

fn primary_expr(rules: Tok) -> Expr {
    let mut inner: Tik = rules.into_inner();
    let the_rule: Tok = inner.next().unwrap();
    let expr = match the_rule.as_rule() {
        Rule::ident => Expr::Var(ident(the_rule)),
        Rule::cons => Expr::Cons(ident(the_rule)),
        Rule::meta => Expr::Meta(ident(the_rule)),
        Rule::no_cases => Expr::Whatever(From::from(the_rule.as_span())),
        Rule::case_expr => case_expr(the_rule),
        Rule::lambda => lambda(the_rule),
        Rule::record => variant_record(the_rule, VarRec::Record),
        Rule::variant => variant_record(the_rule, VarRec::Variant),
        Rule::record_kind => variant_record_kind(the_rule, VarRec::Record),
        Rule::variant_kind => variant_record_kind(the_rule, VarRec::Variant),
        Rule::record_literal => record(the_rule),
        Rule::type_keyword => type_keyword(the_rule),
        Rule::expr => expr(the_rule),
        e => panic!("Unexpected rule: {:?} with token {}", e, the_rule.as_str()),
    };
    end_of_rule(&mut inner);
    expr
}

fn one_param(rules: Tok, plicit: Plicit) -> Param {
    let mut inner: Tik = rules.into_inner();
    let (names, expr) = next_rule!(inner, multi_param);
    let ty = expr.unwrap();
    end_of_rule(&mut inner);
    Param { plicit, names, ty }
}

fn param(rules: Tok) -> Param {
    let mut inner: Tik = rules.into_inner();
    let the_rule: Tok = inner.next().unwrap();
    let param = match the_rule.as_rule() {
        Rule::explicit => one_param(the_rule, Plicit::Ex),
        Rule::implicit => one_param(the_rule, Plicit::Im),
        rule_type => Param {
            plicit: Plicit::Ex,
            names: Vec::with_capacity(0),
            ty: match rule_type {
                Rule::dollar_expr => dollar_expr(the_rule),
                Rule::pi_expr => pi_expr(the_rule),
                e => panic!("Unexpected rule: {:?} with token {}", e, the_rule.as_str()),
            },
        },
    };
    end_of_rule(&mut inner);
    param
}

many_prefix_parser!(pi_expr_internal, Param, param, dollar_expr);
many_prefix_parser!(sig_expr_internal, Param, param, pi_expr);
many_prefix_parser!(multi_param, Ident, ident, expr);
many_prefix_parser!(lambda_internal, Ident, ident, expr);

fn pi_expr(rules: Tok) -> Expr {
    let (params, ret) = pi_expr_internal(rules);
    let ret = ret.unwrap();
    if params.is_empty() {
        ret
    } else {
        Expr::pi(params, ret)
    }
}

fn sig_expr(rules: Tok) -> Expr {
    let (params, ret) = sig_expr_internal(rules);
    let ret = ret.unwrap();
    if params.is_empty() {
        ret
    } else {
        Expr::sig(params, ret)
    }
}

fn case_expr(rules: Tok) -> Expr {
    let mut inner: Tik = rules.into_inner();
    let label = next_ident(&mut inner);
    let binding = next_ident(&mut inner);
    let body = next_rule!(inner, expr);
    let rest = next_rule!(inner, expr);
    end_of_rule(&mut inner);
    Expr::cases(label, binding, body, rest)
}

fn lambda(rules: Tok) -> Expr {
    let syntax_info = SyntaxInfo::from(rules.as_span());
    let (params, ret) = lambda_internal(rules);
    let ret = ret.unwrap();
    Expr::lam(syntax_info, params, ret)
}

fn type_keyword(rules: Tok) -> Expr {
    let syntax_info: SyntaxInfo = From::from(rules.as_span());
    let mut inner: Tik = rules.into_inner();
    let level_ast_node: Tok = inner.next().unwrap();
    debug_assert_eq!(level_ast_node.as_rule(), Rule::type_level);
    let level = Level::Num(level_ast_node.as_str().parse().unwrap_or(0));
    end_of_rule(&mut inner);
    Expr::Type(syntax_info, level)
}

fn ident(rule: Tok) -> Ident {
    Ident {
        text: rule.as_str().to_owned(),
        info: From::from(rule.as_span()),
    }
}
