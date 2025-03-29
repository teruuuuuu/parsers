use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn and_parser<'a, A, F, B, G>(parser1: Parser<'a, A, F>, parser2: Parser<'a, B, G>) -> Parser<'a, (A,B), impl Fn(&'a [u8], usize) -> ParseResult<(A,B)> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    B: Clone + 'a,
    G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
{
    let f1 = parser1.parse.clone();
    let f2 = parser2.parse.clone();

    let f = move |input: &'a [u8], location: usize| {
        match f1(input, location) {
            ParseResult::Success { value: value_a, location: location_a } => {
                match f2(input, location_a) {
                    ParseResult::Success { value: value_b, location: location_b } => {
                        ParseResult::Success { value: (value_a, value_b), location: location_b }
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

#[test]
fn test_and() {
    use crate::core::util::char_parser::char_parser;

    let parser = and_parser(char_parser('a'), char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(('a', 'b'), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse("aacd".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            // assert_eq!(('a', 'b'), value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}


