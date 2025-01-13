use std::rc::Rc;

use crate::core::either::Either;
use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn either_parser<'a, A, F, B, G>(parser1: Parser<'a, A, F>, parser2: Parser<'a, B, G>) -> Parser<'a, Either<A, B>, impl Fn(&'a [u8], usize) -> ParseResult<Either<A,B>> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    B: Clone + 'a,
    G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
{
    let f1 = parser1.parse;
    let f2 = parser2.parse;
    let f = move |input: &'a [u8], location: usize| {
        match f1(input, location) {
            ParseResult::Success { value, location } => {
                ParseResult::Success { value: Either::Left(value), location }
            }
            ParseResult::Failure { parse_error: parse_error1, location: _ } => {
                match f2(input, location) {
                    ParseResult::Success { value, location } => {
                        ParseResult::Success { value: Either::Right(value), location }
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
fn either_test() {
    use crate::core::util::float_parser::float_parser;
    use crate::core::util::number_parser::number_parser;

    let parser = either_parser(float_parser(), number_parser());
    match parser.parse("abcd1234.567a".as_bytes(), 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            match value {
                Either::Left(v) => {
                    assert_eq!(1234.567, v);
                }
                Either::Right(_) => {
                    assert!(false)
                }
            };
            assert_eq!(12, location)
        }
        _ => assert!(false),
    }

    match parser.parse("abcd1234s".as_bytes(), 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            match value {
                Either::Left(_) => {
                    assert!(false)
                }
                Either::Right(v) => {
                    assert_eq!(1234, v);
                }
            };
            assert_eq!(8, location)
        }
        _ => assert!(false),
    }
}


