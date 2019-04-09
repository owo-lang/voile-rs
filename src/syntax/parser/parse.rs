use crate::syntax::parser::concrete::{Declaration, Expression, Identifier, NamedExpression};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "syntax/parser/grammar.pest"]
/// The name stands for "Voile's Parser"
struct VoileParser;

// Tik♂Tok on the clock but the party don't stop!
type Tok<'a> = Pair<'a, Rule>;
type Tik<'a> = Pairs<'a, Rule>;

/// Parse a string into an optional expression based on `file` rule:
/// ```ignore
/// file = { WHITESPACE* ~ expression }
/// ```
pub fn parse_str(input: &str) -> Result<Vec<Declaration>, String> {
    let the_rule: Tok = VoileParser::parse(Rule::file, input)
        .map_err(|err| format!("Parse failed at:{}", err))?
        .next()
        .unwrap();
    let end_pos = the_rule.as_span().end_pos().pos();
    let expression = declarations(the_rule);
    if end_pos < input.len() {
        let rest = &input[end_pos..];
        Err(format!("Does not consume the following code:\n{}", rest))
    } else {
        Ok(expression)
    }
}

macro_rules! next_rule {
    ($inner:expr, $rule_name:ident, $function:ident) => {{
        let token = $inner.next().unwrap();
        debug_assert_eq!(token.as_rule(), Rule::$rule_name);
        $function(token)
    }};
}

#[inline]
fn next_identifier(inner: &mut Tik) -> Identifier {
    next_rule!(inner, identifier, identifier)
}

#[inline]
fn next_expression(inner: &mut Tik) -> Expression {
    next_rule!(inner, expression, expression)
}

#[inline]
fn end_of_rule(inner: &mut Tik) {
    debug_assert_eq!(inner.next(), None)
}

fn declarations(the_rule: Tok) -> Vec<Declaration> {
    let mut decls: Vec<Declaration> = Default::default();
    for prefix_parameter in the_rule.into_inner() {
        decls.push(declaration(prefix_parameter));
    }
    decls
}

fn declaration(rules: Tok) -> Declaration {
    let the_rule: Tok = rules.into_inner().next().unwrap();
    match the_rule.as_rule() {
        Rule::signature => Declaration::Sign(named_expression(the_rule)),
        Rule::implementation => Declaration::Impl(named_expression(the_rule)),
        _ => unreachable!(),
    }
}

fn named_expression(rules: Tok) -> NamedExpression {
    let mut inner: Tik = rules.into_inner();
    let identifier = next_identifier(&mut inner);
    let expression = next_expression(&mut inner);
    end_of_rule(&mut inner);
    NamedExpression {
        name: identifier,
        body: expression,
    }
}

fn expression(rules: Tok) -> Expression {
    let the_rule: Tok = rules.into_inner().next().unwrap();
    match the_rule.as_rule() {
        Rule::identifier => Expression::Var(identifier(the_rule).info),
        e => panic!("Unexpected rule: {:?}", e),
    }
}

fn identifier(rule: Tok) -> Identifier {
    Identifier {
        info: From::from(rule.as_span()),
    }
}

#[cfg(test)]
mod tests {
    use super::super::parse_str_err_printed;

    #[test]
    fn simple_declaration_parsing() {
        parse_str_err_printed("val a : b").unwrap();
        parse_str_err_printed("a : b").unwrap_err();
        parse_str_err_printed("let a = b").unwrap();
        parse_str_err_printed("a = b").unwrap_err();
    }
}
