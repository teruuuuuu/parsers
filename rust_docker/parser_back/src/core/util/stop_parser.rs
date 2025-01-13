use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn stop_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, String, impl Fn(&'a [u8], usize) -> ParseResult<String> + Clone + 'a> 
where 
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();
    let f = move |input: &'a [u8], location: usize| {
        let mut ret: Vec<_> = Vec::new();
        let mut current_location = location;
        let mut parse_success = false;
        loop {
            if current_location >= input.len() {
                parse_success = false;
                break;
            } else {
                match f1(input, current_location) {
                    ParseResult::Success { value: _, location: _ } => {
                        parse_success = true;
                        break;
                    },
                    ParseResult::Failure { parse_error: _, location: _ } => {
                        ret.push(input[current_location] as char);
                        current_location += 1;
                    }
                }
            }
        }
        if parse_success {
            ParseResult::Success { value: ret.into_iter().collect(), location: current_location }
        } else {
            ParseResult::Failure {
                parse_error: ParseError {
                    label: "stop".to_string(),
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


#[test]
fn stop_test() {
    use crate::core::util::char_parser::char_parser;

    let parser = stop_parser(char_parser('"'));
    match parser.parse("abcd\"efg".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!("abcd", value);
            assert_eq!(4, location)
        }
        _ => assert!(false),
    }

    match parser.parse("abcdefg".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(0, location)
        }
        ParseResult::Success { value:_, location:_ } => {
            assert!(false);
        }
    }
}