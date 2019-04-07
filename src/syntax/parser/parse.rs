use crate::syntax::parser::concrete::Declaration;
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
        .unwrap()
        .into_inner()
        .next()
        .unwrap();
    let end_pos = the_rule.as_span().end_pos().pos();
    let expression = unimplemented!();
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
