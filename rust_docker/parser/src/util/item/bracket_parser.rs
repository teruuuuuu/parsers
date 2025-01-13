use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn bracket_parser<'a, A, B>(parser: Parser<'a, A>, l_bracket_parser: Parser<'a, B>, r_bracket_parser: Parser<'a, B>) -> Parser<'a, A>
where
    A: Clone + 'a,
    B: Clone + 'a,
{
    let f1 = parser.parse_func.clone();
    let l_f = l_bracket_parser.parse_func.clone();
    let r_f = r_bracket_parser.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {

        match (l_f)(input, location) {
            ParseResult::Success { value: _, location: location_a } => {
                match (f1)(input, location_a) {
                    ParseResult::Success { value: value_a, location: location_b } => {
                        match (r_f)(input, location_b) {
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

