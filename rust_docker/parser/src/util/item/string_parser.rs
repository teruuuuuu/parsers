use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn string_parser<'a>(str: String) -> Parser<'a, String> {
    let len = str.as_bytes().len();
    let f = move |input: &'a [u8], location: usize| {
        let cur = &input[location..];
        if cur.starts_with(str.as_bytes())  {
            ParseResult::Success { value: str.to_string(), location: location + len}
        } else {
            ParseResult::Failure {
                parse_error: ParseError {
                    label: "string".to_string(),
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
fn string_test() {
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    // use crate::util::parser_methods::ParserMethods;

    let parser = ParserGen::str("bcd".to_owned());
    match parser.parse("abcd", 1) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!("bcd", value);
            assert_eq!(4, location)
        }
        _ => assert!(false),
    }

    let parser = ParserGen::str("bcde".to_owned());
    match parser.parse("abcd", 1) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }

}