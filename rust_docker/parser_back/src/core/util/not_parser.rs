use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn not_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> 
where 
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();
    let f = move |input: &'a [u8], location: usize| {
        if input.len() <= location {
            ParseResult::Failure {
                parse_error: ParseError::new("not".to_string(), "reach end".to_string(), location, vec![]),
                location,
            }
        } else {
            match f1(input, location) {
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

