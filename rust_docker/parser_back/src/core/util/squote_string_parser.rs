use crate::core::either::Either;
use crate::core::parser::{Parser, ParserFunctor};
use crate::core::parse_result::ParseResult;

use crate::core::util::bracket_parser::bracket_parser;
use crate::core::util::char_parser::char_parser;
use crate::core::util::escape_parser::escape_squote;
use crate::core::util::either_parser::either_parser;
use crate::core::util::not_parser::not_parser;
use crate::core::util::seq0_parser::seq0_parser;


pub fn squote_string_parser<'a>() -> Parser<'a, String, impl Fn(&'a [u8], usize) -> ParseResult<String> + Clone + 'a>
{

    let bracket = char_parser('\'');

    let parser = seq0_parser(either_parser(escape_squote(), not_parser(char_parser('\''))).map(|c| match c {
        Either::Left(c) => c,
        Either::Right(c) => c,
    })).map(|v| v.into_iter().collect::<String>());

    bracket_parser(parser, bracket.clone(), bracket.clone())
}


#[test]
fn test_squte_string() {
    let parser = squote_string_parser();



    match parser.parse("'ab\\'cd'efg".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!([97, 98, 39, 99, 100], value.as_bytes());
            assert_eq!(8, location)
        }
        _ => assert!(false),
    }

    match parser.parse("'ab\\cd".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(6, location)
        }
        _ => assert!(false),
    }

    match parser.parse("ab\\'cd\"".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(0, location)
        }
        _ => assert!(false),
    }
}