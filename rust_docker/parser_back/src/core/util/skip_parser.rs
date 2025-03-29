use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn skip_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, (), impl Fn(&'a [u8], usize) -> ParseResult<()> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {
        match f1(input, location) {
            ParseResult::Success { value: _, location } => ParseResult::Success { value: (), location },
            ParseResult::Failure { parse_error, location } => ParseResult::Failure { parse_error, location }
        }
    };

    Parser::new(Rc::new(f))
}