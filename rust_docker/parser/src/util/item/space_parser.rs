use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn space_parser<'a>() -> Parser<'a, char>
{

    let f = move |input: &'a [u8], location: usize| {
        if input.len() <= location {
            ParseResult::Failure {
                parse_error: crate::core::parse_error::ParseError::new("space".to_string(), "reach end".to_string(), location, vec![]),
                location,
            }
        } else {
            let c1 = input[location] as char;
            if c1 == ' ' || c1 == '\t' || c1 == '\n' {
                ParseResult::Success { value: c1, location: location + 1}
            } else {
                ParseResult::Failure {
                    parse_error: crate::core::parse_error::ParseError::new("space".to_string(), "not space".to_string(), location, vec![]),
                    location,
                }
            }
        }
    };

    Parser::new(Rc::new(f))
}