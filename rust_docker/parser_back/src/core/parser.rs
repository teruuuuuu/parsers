use crate::core::parse_result::ParseResult;
use std::rc::Rc;

pub struct Parser<'a, A, F>
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    pub parse: Rc<F>,
    _marker: std::marker::PhantomData<&'a A>,
}

impl<'a, A, F> Parser<'a, A, F>
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    pub fn new(parse: Rc<F>) -> Parser<'a, A, F> {
        Parser { 
            parse,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn parse(&self, input: &'a [u8], location: usize) -> ParseResult<A> {
        (self.parse)(input, location)
    }
}

impl<'a, A, F> Clone for Parser<'a, A, F>
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + Clone + 'a,
{
    fn clone(&self) -> Self {
        Self {
            parse: self.parse.clone(),
            _marker: std::marker::PhantomData,
        }
    }
}

pub trait ParserTrait<'a> {
    type Output;
    type ParserNext<X, G>: ParserTrait<'a, Output = X>
    where
        X: 'a + Clone,
        G: Fn(&'a [u8], usize) -> ParseResult<X> + 'a;

    fn parse(&self, input: &'a [u8], location: usize) -> ParseResult<Self::Output>;
}

impl<'a, A, F> ParserTrait<'a> for Parser<'a, A, F>
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    type Output = A;
    type ParserNext<X, G> = Parser<'a, X, G>
    where
        X: 'a + Clone,
        G: Fn(&'a [u8], usize) -> ParseResult<X> + 'a;

    fn parse(&self, input: &'a [u8], location: usize) -> ParseResult<Self::Output> {
        (self.parse)(input, location)
    }
}

pub trait ParserFunctor<'a>: ParserTrait<'a> {
    fn map<B, G>(self, f: G) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(Self::Output) -> B + 'a;
}

impl<'a, A, F> ParserFunctor<'a> for Parser<'a, A, F>
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    fn map<B, G>(self, f: G) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(Self::Output) -> B + 'a
    {
        let f = move |input, location| match self.parse(input, location) {
            ParseResult::Success { value, location } => ParseResult::successful(f(value), location),
            ParseResult::Failure { parse_error, location } => ParseResult::failure(parse_error, location),
        };
        Parser::new(Rc::new(f))
    }
}

pub trait ParserMonad<'a>: ParserFunctor<'a> {
    fn flat_map<B, G, H>(self, f: H) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        H: Fn(Self::Output) -> Parser<'a, B, G> + 'a;

}

impl<'a, A, F> ParserMonad<'a> for Parser<'a, A, F> 
where
    A: 'a + Clone,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
{
    fn flat_map<B, G, H>(self, f: H) -> Self::ParserNext<B, impl Fn(&'a [u8], usize) -> ParseResult<B> + 'a>
    where
        Self::Output: Clone + 'a,
        B: Clone + 'a,
        G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
        H: Fn(Self::Output) -> Parser<'a, B, G> + 'a,
    {
        let f = move |input: &'a [u8], location: usize| match self.parse(input, location) {
            ParseResult::Success { value, location } => {
                f(value).parse(input, location)
            },  
            ParseResult::Failure { parse_error, location } => {
                ParseResult::Failure { parse_error, location }
            }
        };
        Parser::new(Rc::new(f))
    }

}
