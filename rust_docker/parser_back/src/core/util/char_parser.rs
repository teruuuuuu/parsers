use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;


pub fn char_parser<'a>(c: char) -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> {
    let cu8 = c as u8;
    let f = move |input: &'a [u8], location: usize| {
        if location < input.len() && input[location] == cu8 {
            ParseResult::Success { value: c, location: location + 1}
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


#[test]
fn char_test() {
    use crate::core::parser_methods::ParserMethods;

    let parser = char_parser('a');
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('a', value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }

    let parser = char_parser('a').and(char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(('a', 'b'), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    // let parser = char('a').seq1() + end();
    // match parser.parse("aaaa", 0) {
    //     ParseResult::Success { value, location } => {
    //         assert!(true);
    //         assert_eq!((vec!['a', 'a', 'a', 'a'], ()), value);
    //         assert_eq!(4, location);
    //     }
    //     _ => assert!(false),
    // }

    // let parser = (char('🍣') | char('🍺')).seq0() + end();
    // match parser.parse("🍣🍣🍺🍺🍣🍺🍺", 0) {
    //     ParseResult::Success { value, location } => {
    //         assert!(true);
    //         assert_eq!((vec!['🍣', '🍣', '🍺', '🍺', '🍣', '🍺', '🍺'], ()), value);
    //         assert_eq!(7, location);
    //     }
    //     _ => assert!(false),
    // }
}