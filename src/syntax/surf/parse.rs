use super::ast::Param;
use super::{Decl, DeclKind, Expr, Ident};
use crate::syntax::common::{Level, ParamKind, SyntaxInfo};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "syntax/surf/grammar.pest"]
/// The name stands for "Voile's Parser"
struct VoileParser;

// Tik♂Tok on the clock but the party don't stop!
type Tok<'a> = Pair<'a, Rule>;
type Tik<'a> = Pairs<'a, Rule>;

/// Parse a string into an optional expr based on `file` rule:
/// ```ignore
/// file = { WHITESPACE* ~ expr }
/// ```
pub fn parse_str(input: &str) -> Result<Vec<Decl>, String> {
    let the_rule: Tok = VoileParser::parse(Rule::file, input)
        .map_err(|err| format!("Parse failed at:{}", err))?
        .next()
        .unwrap();
    let end_pos = the_rule.as_span().end_pos().pos();
    let expr = declarations(the_rule);
    if end_pos < input.len() {
        let rest = &input[end_pos..];
        Err(format!("Does not consume the following code:\n{}", rest))
    } else {
        Ok(expr)
    }
}

macro_rules! next_rule {
    ($inner:expr, $rule_name:ident) => {{
        let token = $inner.next().unwrap();
        debug_assert_eq!(token.as_rule(), Rule::$rule_name);
        $rule_name(token)
    }};
}

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
        fn $name(rules: Tok) -> (Vec<$prefix_ty>, Expr) {
            let mut prefixes = vec![];
            let mut end = None;
            for the_rule in rules.into_inner() {
                match the_rule.as_rule() {
                    Rule::$prefix => prefixes.push($prefix(the_rule)),
                    Rule::$end => end = Some($end(the_rule)),
                    e => panic!("Unexpected rule: {:?} with token {}", e, the_rule.as_str()),
                }
            }
            // According to the grammar, `end` cannot be `None` (otherwise it's a pest bug).
            (prefixes, end.unwrap())
        }
    };
}

#[inline]
fn next_ident(inner: &mut Tik) -> Ident {
    next_rule!(inner, ident)
}

#[inline]
fn end_of_rule(inner: &mut Tik) {
    debug_assert_eq!(inner.next(), None)
}

fn declarations(the_rule: Tok) -> Vec<Decl> {
    let mut decls: Vec<Decl> = Default::default();
    for prefix_parameter in the_rule.into_inner() {
        decls.push(declaration(prefix_parameter));
    }
    decls
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
    let expr = next_rule!(inner, expr);
    end_of_rule(&mut inner);
    Decl {
        kind,
        name,
        body: expr,
    }
}

expr_parser!(dollar_expr, comma_expr, app);
expr_parser!(comma_expr, pipe_expr, tup);
expr_parser!(pipe_expr, sum_expr, pipe);
expr_parser!(sum_expr, app_expr, sum);
expr_parser!(app_expr, primary_expr, app);

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
        Rule::bottom => Expr::Bot(ident(the_rule)),
        Rule::one_sum => Expr::ConsType(ident(the_rule)),
        Rule::meta => Expr::Meta(ident(the_rule)),
        Rule::lambda => lambda(the_rule),
        Rule::type_keyword => type_keyword(the_rule),
        Rule::expr => expr(the_rule),
        e => panic!("Unexpected rule: {:?} with token {}", e, the_rule.as_str()),
    };
    end_of_rule(&mut inner);
    expr
}

fn one_param(rules: Tok, kind: ParamKind) -> Param {
    let mut inner: Tik = rules.into_inner();
    let (names, expr) = next_rule!(inner, multi_param);
    end_of_rule(&mut inner);
    Param {
        kind,
        names,
        ty: expr,
    }
}

fn param(rules: Tok) -> Param {
    let mut inner: Tik = rules.into_inner();
    let the_rule: Tok = inner.next().unwrap();
    let param = match the_rule.as_rule() {
        Rule::explicit => one_param(the_rule, ParamKind::Explicit),
        Rule::implicit => one_param(the_rule, ParamKind::Implicit),
        rule_type => {
            let ty = match rule_type {
                Rule::dollar_expr => dollar_expr(the_rule),
                Rule::pi_expr => pi_expr(the_rule),
                e => panic!("Unexpected rule: {:?} with token {}", e, the_rule.as_str()),
            };
            Param {
                names: Vec::with_capacity(0),
                kind: ParamKind::Explicit,
                ty,
            }
        }
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
    if params.is_empty() {
        ret
    } else {
        Expr::pi(params, ret)
    }
}

fn sig_expr(rules: Tok) -> Expr {
    let (params, ret) = sig_expr_internal(rules);
    if params.is_empty() {
        ret
    } else {
        Expr::sig(params, ret)
    }
}

fn lambda(rules: Tok) -> Expr {
    let (params, ret) = lambda_internal(rules);
    Expr::lam(params, ret)
}

fn type_keyword(rules: Tok) -> Expr {
    let syntax_info: SyntaxInfo = From::from(rules.as_span());
    let mut inner: Tik = rules.into_inner();
    let level_ast_node: Tok = inner.next().unwrap();
    debug_assert_eq!(level_ast_node.as_rule(), Rule::type_level);
    let level: Level = level_ast_node.as_str().parse().unwrap();
    end_of_rule(&mut inner);
    Expr::Type(syntax_info, level)
}

fn ident(rule: Tok) -> Ident {
    Ident {
        info: From::from(rule.as_span()),
    }
}

#[cfg(test)]
mod tests {
    use super::super::parse_str_err_printed;

    #[test]
    fn simple_declaration_parsing() {
        parse_str_err_printed("val a : b;").unwrap();
        parse_str_err_printed("val a : b").unwrap_err();
        parse_str_err_printed("a : b").unwrap_err();
        parse_str_err_printed("let a = b;").unwrap();
        parse_str_err_printed("a = b").unwrap_err();
        parse_str_err_printed("a = !").unwrap_err();
    }

    #[test]
    fn primary_expr_parsing() {
        parse_str_err_printed("let a = Type233;").unwrap();
        parse_str_err_printed("let zero = 'Zero;").unwrap();
        parse_str_err_printed("let van = (Type233);").unwrap();
        parse_str_err_printed("let darkholm = (Type233;").unwrap_err();
    }

    #[test]
    fn pi_type_parsing() {
        parse_str_err_printed("val mayori : monika -> (a: A) -> {b : B} -> (c d e : CDE) -> F;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("val star : platinum * (a: A) * {b : B} * F;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
    }

    #[test]
    fn simple_expr_parsing() {
        parse_str_err_printed("let kirisame = utsuho+hakurei;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let reimu = marisa|>alice;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let madoka = homura sayaka kyoko;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let touma = \\kazusa Setsuna -> Ogiso;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let komeji = satori$koishi orin$okku;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let reiuji = 'Cons herrington;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("let scarlet = devil, mansion;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
        parse_str_err_printed("val deep : @Dark Fantasy;")
            .map(|ast| println!("{:?}", ast))
            .unwrap();
    }
}
