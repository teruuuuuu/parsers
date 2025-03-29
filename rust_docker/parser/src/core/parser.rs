// use std::cell::UnsafeCell;
use std::rc::Rc;

use crate::core::parse_result::ParseResult;
pub type Parse<'a, A> = dyn Fn(&'a [u8], usize) -> ParseResult<A> + 'a;

pub struct Parser<'a, A>
{
    pub parse_func: Rc<Parse<'a, A>>,
}

impl<'a, A> Parser<'a, A>
where
    A: 'a + Clone
{
    pub fn new(parse_func: Rc<Parse<'a, A>>) -> Self
    {
        Parser {
            parse_func,
        }
    }

    // pub fn parse(&self, input: &'a str) -> ParseResult<A> {
    //     self.parse_inner(input.as_bytes(), 0)
    // }
    
    // fn parse_inner(&self, input: &'a [u8], location: usize) -> ParseResult<A> {
    //     (self.parse)(input, location)
    // }    
}

impl<'a, A> Clone for Parser<'a, A>
where
    A: 'a + Clone
{
    fn clone(&self) -> Self {
        Self {
            parse_func: self.parse_func.clone(),
        }
    }
}

pub trait ParserTrait<'a, A> {
    // type Output;
    type ParserNext<X>: ParserTrait<'a, X>
    where
        A: 'a + Clone,
        X: 'a + Clone;

    fn parse(&self, input: &'a str, location: usize) -> ParseResult<A> {
        self.parse_inner(input.as_bytes(), location)
    }
    
    fn parse_inner(&self, input: &'a [u8], location: usize) -> ParseResult<A>;

    // fn parse(&self, input: &'a [u8], location: usize) -> ParseResult<Self::Output>;

    // fn parse(&self, input: &'a [u8], location: usize) -> ParseResult<Self::Output>;
}

impl<'a, A> ParserTrait<'a, A> for Parser<'a, A>
where
    A: 'a + Clone,
{
    // type Output = A;
    type ParserNext<X> = Parser<'a, X>
    where
        X: 'a + Clone;

    fn parse_inner(&self, input: &'a [u8], location: usize) -> ParseResult<A> {
        (self.parse_func)(input, location)
    }
}

pub trait ParserFunctor<'a, A>: ParserTrait<'a, A> 
where 
    A: 'a + Clone,
{
    fn map<B, F>(self, f: F) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a,
        F: Fn(A) -> B + 'a;
}

impl<'a, A> ParserFunctor<'a, A> for Parser<'a, A>
where
    A: 'a + Clone,
{
    fn map<B, F>(self, f: F) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a,
        F: Fn(A) -> B + 'a
    {
        let f = move |input, location| match self.parse_inner(input, location) {
            ParseResult::Success { value, location } => ParseResult::successful(f(value), location),
            ParseResult::Failure { parse_error, location } => ParseResult::failure(parse_error, location),
        };
        Parser::new(Rc::new(f))
    }
}

pub trait ParserMonad<'a, A>: ParserFunctor<'a, A> 
where
    A: 'a + Clone,
{
    fn flat_map<B, F>(self, f: F) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a,
        F: Fn(A) -> Parser<'a, B> + 'a;

}

impl<'a, A> ParserMonad<'a, A> for Parser<'a, A> 
where
    A: 'a + Clone,
{
    fn flat_map<B, F>(self, f: F) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a,
        F: Fn(A) -> Parser<'a, B> + 'a,
    {
        let f = move |input: &'a [u8], location: usize| match self.parse_inner(input, location) {
            ParseResult::Success { value, location } => {
                f(value).parse_inner(input, location)
            },  
            ParseResult::Failure { parse_error, location } => {
                ParseResult::Failure { parse_error, location }
            }
        };
        Parser::new(Rc::new(f))
    }

}
