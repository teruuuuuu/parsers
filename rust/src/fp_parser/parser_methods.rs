
use crate::fp_parser::parser::*;

pub enum Either<A,B> {
    Left(A),
    Right(B)
}

pub trait ParserMethods<'a, A>: ParserTrait<'a, A> {
    fn or<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A,B>>
    where
    A: 'a,
    B: 'a;

    fn option(self) -> Parser<'a, Option<A>>
    where A:'a;

    fn and<B>(self, parser2: Parser<'a, B>) -> Parser<'a, (A,B)> 
    where
    A: 'a,
    B: 'a;
}


impl <'a, A>ParserMethods<'a, A> for Parser<'a, A> {

    fn or<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A,B>>
    where
    A: 'a,
    B: 'a {
        Parser::new(move |input, loc| match self.parse(input, loc) {
            ParseResult::ParseOk(result, loc) => ParseResult::ParseOk(Either::Left(result), loc),
            _ => match parser2.parse(input, loc) {
                ParseResult::ParseOk(result, loc) => ParseResult::ParseOk(Either::Right(result), loc),
                ParseResult::ParseNg(message, loc) => ParseResult::ParseNg(message, loc)
            }
        })
    }

    fn option(self) -> Parser<'a, Option<A>>
    where A:'a {
        Parser::new(move |input, loc| match self.parse(input, loc) {
            ParseResult::ParseOk(result, loc) => ParseResult::ParseOk(Option::Some(result), loc),
            _ => ParseResult::ParseOk(Option::None, loc),
        })
    }

    fn and<B>(self, parser2: Parser<'a, B>) -> Parser<'a, (A,B)> 
    where
    A: 'a,
    B: 'a {
        Parser::new(move |input, loc| match self.parse(input, loc) {
            ParseResult::ParseOk(result1, loc1) => match parser2.parse(input, loc1) {
                ParseResult::ParseOk(result2, loc2) => ParseResult::ParseOk((result1, result2), loc2),
                ParseResult::ParseNg(message, loc) => ParseResult::ParseNg(message, loc)
            }
            ParseResult::ParseNg(message, loc) => ParseResult::ParseNg(message, loc)
        })
    }
}