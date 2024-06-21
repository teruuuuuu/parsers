
use crate::fp_parser::parser::*;

pub enum Either<A,B> {
    Left(A),
    Right(B)
}

pub trait ParserMethods<'a, A>: ParserTrait<'a, A> {

    fn or(self, parser2: Parser<'a, A>) -> Parser<'a, A>
    where
    A: 'a;

    fn or_either<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A,B>>
    where
    A: 'a,
    B: 'a;

    fn option(self) -> Parser<'a, Option<A>>
    where A:'a;

    fn and<B>(self, parser2: Parser<'a, B>) -> Parser<'a, (A,B)> 
    where
    A: 'a,
    B: 'a;

    fn not(self) -> Parser<'a, ()>
    where A:'a;

    fn repeat(self) -> Parser<'a, Vec<A>>
    where
    A: 'a;
}


impl <'a, A>ParserMethods<'a, A> for Parser<'a, A> {

    fn or(self, parser2: Parser<'a, A>) -> Parser<'a, A>
    where
    A: 'a {
        Parser::new(move |input, loc| match self.parse(input, loc) {
            ParseResult::ParseOk(result, loc) => ParseResult::ParseOk(result, loc),
            _ => match parser2.parse(input, loc) {
                ParseResult::ParseOk(result, loc) => ParseResult::ParseOk(result, loc),
                ParseResult::ParseNg(message, loc) => ParseResult::ParseNg(message, loc)
            }
        })
    }

    fn or_either<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A,B>>
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
    
    fn not(self) -> Parser<'a, ()>
    where A:'a {
        Parser::new(move |input, loc| match self.parse(input, loc) {
            ParseResult::ParseOk(_, _) => ParseResult::ParseNg("predict not".to_owned(), loc),
            _ => ParseResult::ParseOk((), loc),
        })
    }

    fn repeat(self) -> Parser<'a, Vec<A>>
    where
    A: 'a {
        Parser::new(move |input, loc| {
            let mut res = Vec::<A>::new();
            let mut res_loc = loc;

            while match self.parse(input, res_loc) {
                ParseResult::ParseOk(r, loc) => {
                    res.push(r);
                    res_loc = loc;
                    true
                }
                _ => false
            } {}

            ParseResult::ParseOk(res, res_loc)
        })
    }
}


#[test]
fn test_repeat1() {
    use crate::fp_parser::common_parser::char_parser;

    let p = char_parser('a').repeat().map(|r| r.iter().collect::<String>());
    let input = "aaaaaaaaabcd";


    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            assert_eq!("aaaaaaaaa", r);
            assert_eq!(Loc(9, 12), loc);
            assert!(true)
        }
        _ => assert!(false)
    }
}

#[test]
fn test_repeat2() {
    use crate::fp_parser::common_parser::any_parser;
    use crate::fp_parser::common_parser::char_parser;


    let p = (char_parser('"').not() + any_parser())
        .map(|a| a.1).repeat().map(|a| a.iter().collect::<String>());
    let input = "aaaaaaaaa\"bc";

    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            assert_eq!("aaaaaaaaa", r);
            assert_eq!(Loc(9, 12), loc);
            assert!(true)
        }
        _ => assert!(false)
    }
}