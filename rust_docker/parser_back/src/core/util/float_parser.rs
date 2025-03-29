use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn float_parser<'a>() -> Parser<'a, f64, impl Fn(&'a [u8], usize) -> ParseResult<f64> + Clone + 'a> {
    let f = move |input: &'a [u8], location: usize| {
        let mut sign = 1;
        let mut number = 0;
        let mut current_location = 0;

        let mut decimal = 0;
        let mut decimal_digit = 1;

        if input.len() <= location {
            return ParseResult::failure(
                ParseError::new("float".to_string(), "not float".to_string(), location, vec![]),
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
                ParseError::new("float".to_string(), "not float".to_string(), location, vec![]),
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

        if input_location_len < current_location + 2 || input_location[current_location] < b'.' || 
                input_location[current_location+1] < b'0' || input_location[current_location+1] > b'9' {
            return ParseResult::failure(
                ParseError::new("float".to_string(), "not float".to_string(), location, vec![]),
                current_location);
        } else {
            current_location += 1;
        }

        loop {
            if input_location_len <= current_location || (input_location[current_location] < b'0' || input_location[current_location] > b'9') {
                break
            } else {
                decimal = decimal * 10 + ((input_location[current_location] - b'0') as i64);
                decimal_digit *= 10;
                current_location += 1;
            }
        }

        ParseResult::Success { value: sign as f64 * (number as f64 + (decimal as f64 / decimal_digit as f64)), location: location + current_location}
    };


    Parser::new(Rc::new(f))
}

#[test]
fn float_test() {
    let parser = float_parser();
    match parser.parse("abcd+1234567890.1234567efg".as_bytes(), 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(1234567890.1234567, value);
            assert_eq!(23, location)
        }
        _ => assert!(false),
    }

    match parser.parse("abcd-123456789.01234567efg".as_bytes(), 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(-123456789.01234567, value);
            assert_eq!(23, location)
        }
        _ => assert!(false),
    }

    match parser.parse("abcd-0.0efg".as_bytes(), 4) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(0.0, value);
            assert_eq!(8, location)
        }
        _ => assert!(false),
    }

}