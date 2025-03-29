use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_error::ParseError;
use crate::core::parse_result::ParseResult;

pub fn separate1_parser<'a, A, B>(parser: Parser<'a, A>, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
where
    A: Clone + 'a,
    B: Clone + 'a,
{
    let f1 = parser.parse_func.clone();
    let f2 = separate_parser.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {
        let mut ret: Vec<_> = Vec::new();
        let mut current_location = location;
        let mut is_end = false;

        match (f1)(input, current_location) {
            ParseResult::Success { value, location } => {
                ret.push(value);
                current_location = location
            },
            ParseResult::Failure { parse_error: _, location: _ } => {
                is_end = true;
            }
        }

        while !is_end {
            match (f2)(input, current_location) {
                ParseResult::Success { value: _, location } => {
                    current_location = location;
                    match (f1)(input, current_location) {
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