use pest::Parser;
use pest_derive::Parser;

use voile_util::pest_util::end_of_rule;

use super::common::DBI;

#[derive(Parser)]
#[grammar = "syntax/lisp/lisp.pest"]
struct CoreParser;

#[derive(Debug, Clone)]
pub enum Lisp<'a> {
    Num(DBI),
    Sym(&'a str),
    Many(Vec<Self>),
}

impl<'a> Lisp<'a> {
    pub fn as_dbi(&self) -> Option<DBI> {
        match self {
            Lisp::Num(dbi) => Some(*dbi),
            _ => None,
        }
    }
}

impl<'a> std::fmt::Display for Lisp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Lisp::Many(many) => {
                f.write_str("(")?;
                let mut started = false;
                for lisp in many {
                    if started {
                        f.write_str(" ")?;
                    } else {
                        started = true;
                    }
                    lisp.fmt(f)?;
                }
                f.write_str(")")
            }
            Lisp::Num(dbi) => writeln!(f, "{:?}", dbi),
            Lisp::Sym(str) => writeln!(f, "{}", str),
        }
    }
}

tik_tok!();

define_parse_str!(parse_str, CoreParser, element, element, Lisp);

fn element(rules: Tok) -> Lisp {
    let mut inner: Tik = rules.into_inner();
    let the_rule: Tok = inner.next().unwrap();
    let ret = match the_rule.as_rule() {
        Rule::sym => sym(the_rule),
        Rule::dbi => dbi(the_rule),
        Rule::block => block(the_rule),
        _ => unreachable!(),
    };
    end_of_rule(&mut inner);
    ret
}

fn sym(the_rule: Tok) -> Lisp {
    Lisp::Sym(the_rule.as_str())
}

fn dbi(the_rule: Tok) -> Lisp {
    let s = the_rule.as_str();
    Lisp::Num(DBI(s
        .parse()
        .unwrap_or_else(|_| panic!("Bad DBI: `{}`.", s))))
}

fn block(the_rule: Tok) -> Lisp {
    Lisp::Many(the_rule.into_inner().map(element).collect())
}
