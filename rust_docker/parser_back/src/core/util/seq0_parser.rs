use std::rc::Rc;

use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

pub fn seq0_parser<'a, A, F>(parser: Parser<'a, A, F>) -> Parser<'a, Vec<A>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<A>> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    let f1 = parser.parse.clone();

    let f = move |input: &'a [u8], location: usize| {
        let mut ret: Vec<_> = Vec::new();
        let mut current_location = location;
        loop {
            match f1(input, current_location) {
                ParseResult::Success { value, location } => {
                    ret.push(value);
                    current_location = location
                },
                ParseResult::Failure { parse_error: _, location: _ } => {
                    break;
                }
            }
        }


        ParseResult::Success { value: ret, location: current_location }
    };
    Parser::new(Rc::new(f))
}


#[test]
pub fn test_seq0() {
    use crate::core::parser::ParserFunctor;

    use crate::core::util::char_parser::char_parser;
    use crate::core::util::or_parser::or_parser;

    let parser = seq0_parser(
        or_parser(char_parser('a'), char_parser('b'))
    ).map(|v| v.into_iter().collect::<String>());


    match parser.parse("cababcd".as_bytes(), 1) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!("abab", value);
            assert_eq!(5, location)
        }
        _ => assert!(false),
    }

    match parser.parse("cababcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!("", value);
            assert_eq!(0, location)
        }
        _ => assert!(false),
    }

}