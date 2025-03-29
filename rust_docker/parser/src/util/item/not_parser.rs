use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn not_parser<'a, A>(parser: Parser<'a, A>) -> Parser<'a, char> 
where 
    A: Clone + 'a
{
    let parse_func = parser.parse_func.clone();
    let f = move |input: &'a [u8], location: usize| {
        if input.len() <= location {
            ParseResult::Failure {
                parse_error: ParseError::new("not".to_string(), "reach end".to_string(), location, vec![]),
                location,
            }
        } else {
            match (parse_func)(input, location) {
                ParseResult::Failure { parse_error: _, location } => {
                    ParseResult::Success { value: input[location] as char, location: location + 1}
                }
                ParseResult::Success { value: _, location } => {
                    ParseResult::Failure {
                        parse_error: ParseError::new("not".to_string(), "not".to_string(), location, vec![]),
                        location,
                    }
                }
            }
        }        
        
    };
    Parser::new(Rc::new(f))
}

