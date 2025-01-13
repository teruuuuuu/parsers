use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;

pub fn or_parser<'a, A, F>(parser1: Parser<'a, A, F>, parser2: Parser<'a, A, F>) -> Parser<'a, A, impl Fn(&'a [u8], usize) -> ParseResult<A> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser1.parse;
    let f2 = parser2.parse;
    let f = move |input: &'a [u8], location: usize| {
        match f1(input, location) {
            ParseResult::Success { value, location } => {
                ParseResult::Success { value, location }
            }
            ParseResult::Failure { parse_error: parse_error1, location: _ } => {
                match f2(input, location) {
                    ParseResult::Success { value, location } => {
                        ParseResult::Success { value, location }
                    }
                    ParseResult::Failure { parse_error: parse_error2, location: _ } => {
                        let mut children = vec![];
                        children.push(parse_error1);
                        children.push(parse_error2);
                        let parse_error = ParseError::new("or".to_string(), "not match".to_string(), location, children);
                        ParseResult::Failure { parse_error, location }
                    }
                }
            }
        }
    };
    Parser::new(Rc::new(f))
}

#[test]
fn or_test() {
    use crate::core::util::char_parser::char_parser;

    let parser = or_parser(char_parser('a'), char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('a', value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }

    match parser.parse("bcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('b', value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}


