use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_error::ParseError;
use crate::core::parse_result::ParseResult;

pub fn separate1_parser<'a, A, F, B, G>(parser: Parser<'a, A, F>, separate_parser: Parser<'a, B, G>) -> Parser<'a, Vec<A>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<A>> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    B: Clone + 'a,
    G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
{
    let f1 = parser.parse.clone();
    let f2 = separate_parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {
        let mut ret: Vec<_> = Vec::new();
        let mut current_location = location;
        let mut is_end = false;

        match f1(input, current_location) {
            ParseResult::Success { value, location } => {
                ret.push(value);
                current_location = location
            },
            ParseResult::Failure { parse_error: _, location: _ } => {
                is_end = true;
            }
        }

        while !is_end {
            match f2(input, current_location) {
                ParseResult::Success { value: _, location } => {
                    current_location = location;
                    match f1(input, current_location) {
                        ParseResult::Success { value, location } => {
                            ret.push(value);
                            current_location = location
                        },
                        ParseResult::Failure { parse_error: _, location: _ } => {
                            is_end = true;
                        }
                    }
                },
                ParseResult::Failure { parse_error: _, location: _ } => {
                    is_end = true;
                }
            }
        }
        if ret.len() > 0 {
            ParseResult::Success { value: ret, location: current_location }
        } else {
            ParseResult::Failure {
                parse_error: ParseError {
                    label: "separate1".to_string(),
                    message: "not match".to_string(),
                    location,
                    children: vec![]
                },
                location,
            }
        }
        
    };
    Parser::new(Rc::new(f))
}