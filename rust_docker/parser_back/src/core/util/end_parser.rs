use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn end_parser<'a>() -> Parser<'a, (), impl Fn(&'a [u8], usize) -> ParseResult<()> + Clone + 'a>
{
    let f = move |input: &'a [u8], location: usize| {

        if input.len() == location {
            ParseResult::Success { value: (), location }
        } else {
            ParseResult::Failure {
                parse_error: crate::core::parse_error::ParseError::new("end".to_string(), "not end".to_string(), location, vec![]),
                location,
            }
        }
    };
    Parser::new(Rc::new(f))
}

#[test]
fn end_test() {
    let parser = end_parser();
    match parser.parse("abcdefg".as_bytes(), 7) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!((), value);
            assert_eq!(7, location)
        }
        _ => assert!(false),
    }
}