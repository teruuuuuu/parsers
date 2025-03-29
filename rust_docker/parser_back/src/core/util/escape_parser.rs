use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;

pub fn escape_parser<'a>(escape_char: char) -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> {
    let cu8 = escape_char as u8;
    let f = move |input: &'a [u8], location: usize| {
        if location + 1 < input.len() && input[location] == b'\\' && input[location + 1] == cu8 {
            ParseResult::Success { value: escape_char, location: location + 2}
        } else {
            ParseResult::Failure {
                parse_error: ParseError {
                    label: "char".to_string(),
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

pub fn escape_squote<'a>() -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> {
    escape_parser('\'')
}

pub fn escape_dquote<'a>() -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> {
    escape_parser('"')
}

#[test]
pub fn test_escape() {

    let parser = escape_dquote();

    println!("bytes: {:?}", "ab\\\"cd".as_bytes());

    match parser.parse("ab\\\"cd".as_bytes(), 2) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('"', value);
            assert_eq!(4, location)
        }
        _ => assert!(false),
    }

}