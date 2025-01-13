use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn number_parser<'a>() -> Parser<'a, i64> {
    let f = move |input: &'a [u8], location: usize| {
        let mut sign = 1;
        let mut number = 0;
        let mut current_location = 0;

        if input.len() <= location {
            return ParseResult::failure(
                ParseError::new("number".to_string(), "not number".to_string(), location, vec![]),
                current_location);
        }

        let input_location = &input[location..];
        let input_location_len = input_location.len();

        if input_location[0] == b'-' {
            sign = -1;
            current_location += 1;
        } else if input_location[0] == b'+' {
            sign = 1;
            current_location += 1;
        }
        if input_location_len < current_location || input_location[current_location] < b'0' || input_location[current_location] > b'9' {
            return ParseResult::failure(
                ParseError::new("number".to_string(), "not number".to_string(), location, vec![]),
                current_location);
        }
        loop {
            if input_location_len <= current_location || (input_location[current_location] < b'0' || input_location[current_location] > b'9') {
                break
            } else {
                number = number * 10 + ((input_location[current_location] - b'0') as i64);
                current_location += 1;
            }
        }
        ParseResult::Success { value: sign * number, location: location + current_location}
    };


    Parser::new(Rc::new(f))
}

#[test]
fn number_test() {
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    // use crate::util::parser_methods::ParserMethods;

    let parser = ParserGen::number();
    match parser.parse(&"abcd+12345678901234567efg", 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(12345678901234567, value);
            assert_eq!(22, location)
        }
        _ => assert!(false),
    }

    match parser.parse(&"abcd-12345678901234567efg", 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(-12345678901234567, value);
            assert_eq!(22, location)
        }
        _ => assert!(false),
    }

    match parser.parse(&"abcd-0efg", 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(0, value);
            assert_eq!(6, location)
        }
        _ => assert!(false),
    }

}