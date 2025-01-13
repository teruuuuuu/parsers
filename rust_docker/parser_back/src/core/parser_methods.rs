use std::rc::Rc;

use crate::core::either::Either;
use crate::core::parser::{Parser, ParserFunctor, ParserTrait};
use crate::core::parse_result::ParseResult;
use crate::core::parse_error::ParseError;

use crate::core::util::and_parser::and_parser;
use crate::core::util::array_parser::array_parser;
use crate::core::util::bracket_parser::bracket_parser;
use crate::core::util::either_parser::either_parser;
use crate::core::util::not_parser::not_parser;
use crate::core::util::optional_parser::optional_parser;
use crate::core::util::seq0_parser::seq0_parser;
use crate::core::util::seq1_parser::seq1_parser;
use crate::core::util::skip_parser::skip_parser;
use crate::core::util::separate0_parser::separate0_parser;
use crate::core::util::separate1_parser::separate1_parser;
use crate::core::util::skip_seq0_parser::skip_seq0_parser;
use crate::core::util::skip_space_parser::skip_space_parser;
use crate::core::util::stop_parser::stop_parser;

pub trait ParserMethods<'a>: ParserTrait<'a> 
{
    fn and<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<(Self::Output, B), impl Fn(&'a [u8], usize) -> ParseResult<(Self::Output, B)> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a;
    
    fn and_left<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a;

    fn and_right<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a;
    
    fn array<B, G, C, H>(self, separate_parser: Parser<'a, B, G>, l_bracket_parser: Parser<'a, C, H>, r_bracket_parser: Parser<'a, C, H>) -> 
        Parser<'a, Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + Clone + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        C: Clone + 'a,
        H: Fn(&'a [u8], usize) -> ParseResult<C> + 'a;
    
    fn bracket<B, G>(self, l_bracket: Parser<'a, B, G>, r_bracket: Parser<'a, B, G>) -> Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a;
    
    fn either<B, G>(self, parser2: Parser<'a, B, G>) -> 
        Self::ParserNext<Either<Self::Output, B>, impl Fn(&'a [u8], usize) -> ParseResult<Either<Self::Output, B>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a;
    
    fn not<>(self) -> Self::ParserNext<char, impl Fn(&'a [u8], usize) -> ParseResult<char> + 'a>
    where
        Self::Output: Clone + 'a;
        
    fn or<G>(self, parser2: Parser<'a, Self::Output, G>) -> 
        Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a,
        G: Clone + 'a;
    
    fn optional<>(self) -> 
        Self::ParserNext<Option<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Option<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a;
    
    fn pure<B>(self, ret: B) -> 
        Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a;

    fn separate0<B, G>(self, separate_parser: Parser<'a, B, G>) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a;
    
    fn separate1<B, G>(self, separate_parser: Parser<'a, B, G>) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a;

    fn seq0<>(self) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a;
    
    fn seq1<>(self) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a;
    
    fn skip<>(self) -> 
        Self::ParserNext<(), impl Fn(&'a [u8], usize) -> ParseResult<()> + 'a>
    where
        Self::Output: Clone + 'a;
    
    fn skip_seq0<>(self) -> 
        Self::ParserNext<(), impl Fn(&'a [u8], usize) -> ParseResult<()> + 'a>
    where
        Self::Output: Clone + 'a;
        
    fn stop<>(self) -> 
        Self::ParserNext<String, impl Fn(&'a [u8], usize) -> ParseResult<String> + 'a>
    where
        Self::Output: Clone + 'a;
        
    fn with_skip_space<>(self) -> 
        Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a;

}

impl<'a, A, F> ParserMethods<'a> for Parser<'a, A, F> 
where 
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    F: Clone + 'a
{
    fn and<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<(A, B),  impl Fn(&'a [u8], usize) -> ParseResult<(A, B)> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
    {

        and_parser(self, parser2)
    }

    fn and_left<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a
    {
        self.and(parser2).map(|v| v.0)
    }

    fn and_right<B, G>(self, parser2: Parser<'a, B, G>) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a
    {
        self.and(parser2).map(|v| v.1)
    }

    fn array<B, G, C, H>(self, separate_parser: Parser<'a, B, G>, l_bracket_parser: Parser<'a, C, H>, r_bracket_parser: Parser<'a, C, H>) -> 
        Parser<'a, Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + Clone + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        C: Clone + 'a,
        H: Fn(&'a [u8], usize) -> ParseResult<C> + 'a
    {
        array_parser(self, separate_parser, l_bracket_parser, r_bracket_parser)
    }

    fn bracket<B, G>(self, l_bracket: Parser<'a, B, G>, r_bracket: Parser<'a, B, G>) -> Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        G: Clone + 'a
    {
        bracket_parser(self, l_bracket, r_bracket)
    }

    fn either<B, G>(self, parser2: Parser<'a, B, G>) -> 
        Self::ParserNext<Either<Self::Output, B>, impl Fn(&'a [u8], usize) -> ParseResult<Either<Self::Output, B>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a 
    {
        either_parser(self, parser2)
    }

    fn not<>(self) -> Self::ParserNext<char, impl Fn(&'a [u8], usize) -> ParseResult<char> + 'a>
    where
        Self::Output: Clone + 'a
    {
        not_parser(self)
    }

    fn optional<>(self) -> 
        Self::ParserNext<Option<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Option<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a
    {
        optional_parser(self)
    }

    fn or<G>(self, parser2: Parser<'a, Self::Output, G>) -> 
        Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a,
        G: Clone + 'a
    {

        let f1 = self.parse;
        let f2 = parser2.parse;
        let f = move |input: &'a [u8], location: usize| {
            match f1(input, location) {
                ParseResult::Success { value, location } => {
                    ParseResult::Success { value, location }
                }
                ParseResult::Failure { parse_error: parse_error1, location: _ } => {
                    match f2(input, location) {
                        ParseResult::Success { value, location } => {
                            ParseResult::Success { value, location }
                        }
                        ParseResult::Failure { parse_error: parse_error2, location: _ } => {
                            let mut children = vec![];
                            children.push(parse_error1);
                            children.push(parse_error2);
                            let parse_error = ParseError::new("or".to_string(), "not match".to_string(), location, children);
                            ParseResult::Failure { parse_error, location }
                        }
                    }
                }
            }
        };
        Parser::new(Rc::new(f))
    }

    fn pure<B>(self, ret: B) -> 
        Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a
    {
        self.map(move |_| ret.clone())
    }

    fn separate0<B, G>(self, separate_parser: Parser<'a, B, G>) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a
    {
        separate0_parser(self, separate_parser)
    }
    
    fn separate1<B, G>(self, separate_parser: Parser<'a, B, G>) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a
    {
        separate1_parser(self, separate_parser)
    }

    fn seq0<>(self) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a

    {
        seq0_parser(self)
    }


    fn seq1<>(self) -> 
        Self::ParserNext<Vec<Self::Output>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<Self::Output>> + 'a>
    where
        Self::Output: Clone + 'a
    {
        seq1_parser(self)
    }

    fn skip<>(self) -> 
        Self::ParserNext<(), impl Fn(&'a [u8], usize) -> ParseResult<()> + 'a>
    where
        Self::Output: Clone + 'a
    {
        skip_parser(self)
    }

    fn skip_seq0<>(self) -> 
        Self::ParserNext<(), impl Fn(&'a [u8], usize) -> ParseResult<()> + 'a>
    where
        Self::Output: Clone + 'a
    {
        skip_seq0_parser(self)
    }

    fn stop<>(self) -> 
        Self::ParserNext<String, impl Fn(&'a [u8], usize) -> ParseResult<String> + 'a>
    where
        Self::Output: Clone + 'a
    {
        stop_parser(self)
    }
    
    fn with_skip_space<>(self) -> 
        Self::ParserNext<Self::Output, impl Fn(&'a [u8], usize) -> ParseResult<Self::Output> + 'a>
    where
        Self::Output: Clone + 'a
    {
        skip_space_parser().and_right(self)
    }

}



#[test]
fn test_and() {
    use crate::core::util::char_parser::char_parser;
    
    let parser = char_parser('a').and(char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(('a', 'b'), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse("aacd".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            // assert_eq!(('a', 'b'), value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}

#[test]
fn test_and_left() {
    use crate::core::util::char_parser::char_parser;
    
    let parser = char_parser('a').and_left(char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('a', value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse("aacd".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            // assert_eq!(('a', 'b'), value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}

#[test]
fn test_and_right() {
    use crate::core::util::char_parser::char_parser;
    
    let parser = char_parser('a').and_right(char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('b', value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse("aacd".as_bytes(), 0) {
        ParseResult::Failure { parse_error: _, location } => {
            assert!(true);
            // assert_eq!(('a', 'b'), value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}

#[test]
fn test_or() {
    use crate::core::util::char_parser::char_parser;
    
    let parser = char_parser('a').or(char_parser('b'));
    match parser.parse("abcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('a', value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }

    match parser.parse("bcd".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!('b', value);
            assert_eq!(1, location)
        }
        _ => assert!(false),
    }
}