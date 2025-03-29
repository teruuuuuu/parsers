use crate::core::either::Either;
use crate::core::parser::{Parser, ParserFunctor};
// use crate::core::parse_result::ParseResult;

use crate::util::item::bracket_parser::bracket_parser;
use crate::util::item::char_parser::char_parser;
use crate::util::item::escape_parser::escape_dquote;
use crate::util::item::either_parser::either_parser;
use crate::util::item::not_parser::not_parser;
use crate::util::item::seq0_parser::seq0_parser;


pub fn dquote_string_parser<'a>() -> Parser<'a, String>
{

    let bracket = char_parser('"');

    let parser = seq0_parser(either_parser(escape_dquote(), not_parser(char_parser('"'))).map(|c| match c {
        Either::Left(c) => c,
        Either::Right(c) => c,
    })).map(|v| v.into_iter().collect::<String>());

    bracket_parser(parser, bracket.clone(), bracket.clone())
}


#[test]
fn test_dqute_string() {
    use crate::core::parse_result::ParseResult;
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    // use crate::util::parser_methods::ParserMethods;

    let parser = ParserGen::dquote_string();

    println!("input: {:?}", "ab\\\"cd".as_bytes());

    match parser.parse(&"\"ab\\\"cd\"efg", 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!([97, 98, 34, 99, 100], value.as_bytes());
            assert_eq!(8, location)
        }
        _ => assert!(false),
    }

    match parser.parse(&"\"ab\\cd", 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(6, location)
        }
        _ => assert!(false),
    }

    match parser.parse(&"ab\\\"cd\"", 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(0, location)
        }
        _ => assert!(false),
    }
}