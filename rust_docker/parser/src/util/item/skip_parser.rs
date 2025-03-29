use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn skip_parser<'a, A>(parser: Parser<'a, A>) -> Parser<'a, ()>
where
    A: Clone + 'a,
{
    let f1 = parser.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {
        match (f1)(input, location) {
            ParseResult::Success { value: _, location } => ParseResult::Success { value: (), location },
            ParseResult::Failure { parse_error, location } => ParseResult::Failure { parse_error, location }
        }
        
    };

    Parser::new(Rc::new(f))
}