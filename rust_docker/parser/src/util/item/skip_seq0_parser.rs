use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn skip_seq0_parser<'a, A>(parser: Parser<'a, A>) -> Parser<'a, ()>
where
    A: Clone + 'a,
{
    let f1 = parser.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {
        let mut current_location = location;
        loop {
            match (f1)(input, current_location) {
                ParseResult::Success { value: _, location } => {
                    current_location = location
                },
                ParseResult::Failure { parse_error: _, location: _ } => {
                    break;
                }
            }
        }

        ParseResult::Success { value: (), location: current_location }        
    };

    Parser::new(Rc::new(f))
}