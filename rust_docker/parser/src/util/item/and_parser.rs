use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn and_parser<'a, A, B>(parser1: Parser<'a, A>, parser2: Parser<'a, B>) -> Parser<'a, (A,B)>
where
    A: Clone + 'a,
    B: Clone + 'a,
{
    let parse1_func = parser1.parse_func.clone();
    let parse2_func = parser2.parse_func.clone();

    let f = move |input: &'a [u8], location: usize| {
        match (parse1_func)(input, location) {
            ParseResult::Success { value: value_a, location: location_a } => {
                match (parse2_func)(input, location_a) {
                    ParseResult::Success { value: value_b, location: location_b } => {
                        ParseResult::Success { value: (value_a, value_b), location: location_b }
                    }
                    ParseResult::Failure { parse_error, location } => {
                        ParseResult::Failure { parse_error, location }
                    }
                }
            }
            ParseResult::Failure { parse_error, location } => {
                ParseResult::Failure { parse_error, location }
            }
        }
        
    };
    Parser::new(Rc::new(f))
}

#[test]
fn test_and() {
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    use crate::util::parser_methods::ParserMethods;

    let parser = ParserGen::char('a').and(ParserGen::char('b'));
    match parser.parse(&"abcd", 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(('a', 'b'), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }
    
    match parser.parse("aacd", 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            // assert_eq!(('a', 'b'), value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}


