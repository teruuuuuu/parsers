use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn bracket_parser<'a, A, F, B, G>(parser: Parser<'a, A, F>, l_bracket_parser: Parser<'a, B, G>, r_bracket_parser: Parser<'a, B, G>) -> Parser<'a, A, impl Fn(&'a [u8], usize) -> ParseResult<A> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    B: Clone + 'a,
    G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
{
    let f1 = parser.parse.clone();
    let l_f = l_bracket_parser.parse.clone();
    let r_f = r_bracket_parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {
        match l_f(input, location) {
            ParseResult::Success { value: _, location: location_a } => {
                match f1(input, location_a) {
                    ParseResult::Success { value: value_a, location: location_b } => {
                        match r_f(input, location_b) {
                            ParseResult::Success { value: _, location: location_c } => {
                                ParseResult::Success { value: value_a, location: location_c }
                            }
                            ParseResult::Failure { parse_error, location } => {
                                ParseResult::Failure { parse_error, location }
                            }
                        }
                    }
                    ParseResult::Failure { parse_error, location } => {
                        ParseResult::Failure { parse_error, location }
                    }
                }
            }
            ParseResult::Failure { parse_error, location } => {
                ParseResult::Failure { parse_error, location }
            }
        }
    };
    Parser::new(Rc::new(f))
}

