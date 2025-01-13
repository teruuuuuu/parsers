use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;

pub fn seq1_parser<'a, A>(parser: Parser<'a, A>) -> Parser<'a, Vec<A>>
where
    A: Clone + 'a
{
    let parse1_func = parser.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {
        let mut ret: Vec<_> = Vec::new();
        let mut current_location = location;
        loop {
            match (parse1_func)(input, current_location) {
                ParseResult::Success { value, location } => {
                    ret.push(value);
                    current_location = location
                },
                ParseResult::Failure { parse_error: _, location: _ } => {
                    break;
                }
            }
        }

        if ret.len() == 0 {
            return ParseResult::Failure {
                parse_error: ParseError::new("seq1".to_string(), "not match".to_string(), location, vec![]),
                location
            }
        } else {
            ParseResult::Success { value: ret, location: current_location }
        }
        
    };
    Parser::new(Rc::new(f))
}


#[test]
pub fn test_seq1() {
    // use crate::core::parser::ParserFunctor;

    // use crate::core::util::char_parser::char_parser;
    // use crate::core::util::or_parser::or_parser;

    // let parser = seq1_parser(
    //     or_parser(char_parser('a'), char_parser('b'))
    // ).map(|v| v.into_iter().collect::<String>());


    // match parser.parse("cababcd".as_bytes(), 1) {
    //     ParseResult::Success { value, location } => {
    //         assert!(true);
    //         assert_eq!("abab", value);
    //         assert_eq!(5, location)
    //     }
    //     _ => assert!(false),
    // }

    // match parser.parse("cababcd".as_bytes(), 0) {
    //     ParseResult::Failure { parse_error: _, location } => {
    //         assert!(true);
    //         assert_eq!(0, location)
    //     },
    //     ParseResult::Success { value: _, location: _ } => {
    //         assert!(false);
    //     }
    // }

}