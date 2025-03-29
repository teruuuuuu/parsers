use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn skip_seq0_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, (), impl Fn(&'a [u8], usize) -> ParseResult<()> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {

        let mut current_location = location;
        loop {
            match f1(input, current_location) {
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