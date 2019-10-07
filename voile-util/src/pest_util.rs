use pest::iterators::{Pair, Pairs};
use pest::{Parser, RuleType, Span};

use crate::loc::Loc;

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
macro_rules! many_prefix_parser {
    ($name:ident, $prefix_ty:ident, $prefix:ident, $end:ident, $e:ty) => {
        fn $name(rules: Tok) -> (Vec<$prefix_ty>, Option<$e>) {
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

pub fn strict_parse<'a, P, F, R, T>(rule: R, input: &'a str, f: F) -> Result<T, String>
where
    P: Parser<R>,
    R: RuleType,
    F: FnOnce(Pair<'a, R>) -> T,
{
    let rule = P::parse(rule, input)
        .map_err(|err| format!("Parse failed at:{}", err))?
        .next()
        .unwrap();
    let end_pos = rule.as_span().end_pos();

    if end_pos.pos() < input.len() {
        let rest = &input[end_pos.pos()..];
        Err(format!("Does not consume the following code: '{}'", rest))
    } else {
        Ok(f(rule))
    }
}

#[inline]
pub fn end_of_rule<Rule: RuleType>(inner: &mut Pairs<Rule>) {
    debug_assert_eq!(inner.next(), None);
}

impl<'a> From<Span<'a>> for Loc {
    fn from(span: Span) -> Self {
        Loc {
            line: span.start_pos().line_col().0,
            start: span.start(),
            end: span.end(),
            is_generated: false,
        }
    }
}
