use pest::iterators::Pairs;
use pest::RuleType;

// Tik♂Tok on the clock but the party don't stop!
#[macro_export]
macro_rules! tik_tok {
    () => {
        type Tok<'a> = pest::iterators::Pair<'a, self::Rule>;
        type Tik<'a> = pest::iterators::Pairs<'a, self::Rule>;
    };
}

#[macro_export]
macro_rules! next_rule {
    ($inner:expr, $rule_name:ident) => {{
        let token = $inner.next().unwrap();
        debug_assert_eq!(token.as_rule(), self::Rule::$rule_name);
        $rule_name(token)
    }};
}

#[macro_export]
macro_rules! define_parse_str {
    ($name:ident, $parser:ident, $root_rule:ident, $root_trans:ident, $ret:ty) => {
        /// Parse a string into an optional expr based on the `$root_rule` rule:
        pub fn $name(input: &str) -> Result<$ret, String> {
            let the_rule: Tok = $parser::parse(Rule::$root_rule, input)
                .map_err(|err| format!("Parse failed at:{}", err))?
                .next()
                .unwrap();
            let end_pos = the_rule.as_span().end_pos().pos();
            let expr = $root_trans(the_rule);
            if end_pos < input.len() {
                let rest = &input[end_pos..];
                Err(format!("Does not consume the following code:\n{}", rest))
            } else {
                Ok(expr)
            }
        }
    };
}

#[inline]
pub fn end_of_rule<Rule: RuleType>(inner: &mut Pairs<Rule>) {
    debug_assert_eq!(inner.next(), None);
}
