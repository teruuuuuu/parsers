use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn optional_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, Option<A>, impl Fn(&'a [u8], usize) -> ParseResult<Option<A>> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {

        match f1(input, location) {
            ParseResult::Success { value, location } => ParseResult::successful(Option::Some(value), location),
            ParseResult::Failure { parse_error:_, location} => ParseResult::successful(Option::None, location)
        }
    };
    Parser::new(Rc::new(f))
}


#[test]
fn option_test() {
    use crate::core::util::string_parser::string_parser;

    let parser = optional_parser(string_parser("abcd".to_owned()));
    match parser.parse("abcdefg".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(Option::Some("abcd".to_owned()), value);
            assert_eq!(4, location)
        }
        _ => assert!(false),
    }

    match parser.parse("bcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(Option::None, value);
            assert_eq!(0, location)
        }
        _ => assert!(false),
    }
}


