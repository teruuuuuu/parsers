use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::either::Either;
use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn either_parser<'a, A, B>(parser1: Parser<'a, A>, parser2: Parser<'a, B>) -> Parser<'a, Either<A, B>>
where
    A: Clone + 'a,
    B: Clone + 'a,
{
    let parse1_func = parser1.parse_func.clone();
    let parse2_func = parser2.parse_func.clone();
    let fparse_func = move |input: &'a [u8], location: usize| {
        match (parse1_func)(input, location) {
            ParseResult::Success { value, location } => {
                ParseResult::Success { value: Either::Left(value), location }
            }
            ParseResult::Failure { parse_error: parse_error1, location: _ } => {
                match (parse2_func)(input, location) {
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
    Parser::new(Rc::new(fparse_func))
}

#[test]
fn either_test() {
    use crate::core::parse_result::ParseResult;
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    // use crate::util::parser_methods::ParserMethods;

    

    let parser = ParserGen::either(ParserGen::float(), ParserGen::number());
    match parser.parse(&"abcd1234.567a", 4) {
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

    match parser.parse(&"abcd1234s", 4) {
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


