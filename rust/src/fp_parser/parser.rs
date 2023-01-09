
use std::fmt::*;
use std::ops::Add;
use std::rc::Rc;

use crate::fp_parser::parser_methods::ParserMethods;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Loc(pub usize, pub usize);

impl Loc {
  pub fn merge(&self, other: &Loc) -> Loc {
    use::std::cmp::{max, min};
    Loc(min(self.0, other.0), max(self.1, other.1))
  }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Loc({}, {})", self.0, self.1)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ParseResult<A> {
    ParseOk(A, Loc),
    ParseNg(String, Loc)
}

type Parse<'a, A> = dyn Fn(&'a str, Loc) -> ParseResult<A> + 'a;

pub struct Parser<'a, A> {
    pub parse: Rc<Parse<'a, A>>
}

impl <'a, A>Parser<'a, A> {
    pub fn new<F>(parse: F) -> Parser<'a, A>
    where
      F: Fn(&'a str, Loc) -> ParseResult<A> + 'a {Parser { parse: Rc::new(parse)}}
}

impl <'a, A>Clone for Parser<'a, A> {
    fn clone(&self) -> Self {
        Self {
            parse: self.parse.clone(),
        }
    }
}

pub trait ParserTrait<'a, A> {
    fn parse(&self,input: &'a str, loc: Loc ) -> ParseResult<A>;
}

impl <'a, A>ParserTrait<'a, A> for Parser<'a, A> {
    fn parse(&self,input: &'a str, loc: Loc ) -> ParseResult<A> {
        (self.parse)(input, loc)
    }
}

trait FunctorParser<'a, A>: ParserTrait<'a, A> {
    fn map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(A) -> B + 'a,
      A: 'a;
}

impl <'a, A>FunctorParser<'a, A> for Parser<'a, A> {
    fn map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(A) -> B + 'a,
      A: 'a {
        Parser::new(move |input: &str, loc: Loc| match self.parse(input, loc) {
            ParseResult::ParseOk(r, loc) => ParseResult::ParseOk(f(r), loc),
            ParseResult::ParseNg(m, loc) => ParseResult::ParseNg(m, loc)
        })
      }
}

trait MonadParser<'a, A>: FunctorParser<'a, A> {
    fn flat_map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(A) -> Parser<'a, B> + 'a,
      A: 'a;
}

impl <'a, A>MonadParser<'a, A> for Parser<'a, A> {
    fn flat_map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(A) -> Parser<'a, B> + 'a,
      A: 'a {
        Parser::new(move |input: &str, loc: Loc| match self.parse(input, loc) {
            ParseResult::ParseOk(r, loc) => f(r).parse(input, loc),
            ParseResult::ParseNg(m, loc) => ParseResult::ParseNg(m, loc)
        })
      }
}


impl<'a, A, B> Add<Parser<'a, B>> for Parser<'a, A>
where
  A: 'a,
  B: 'a {
    type Output = Parser<'a, (A,B)>;
    fn add(self, parser2: Parser<'a, B>) -> Self::Output {
        self.and(parser2)
  }
}


#[test]
fn test_char_parser_map() {
    use crate::fp_parser::common_parser::char_parser;

    let p1 = char_parser('a');
    let p2 = p1.map(|c| String::from(c));

    let input = "abcdefg";

    match p2.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(a, loc1) => {
            assert!(true);
            println!("{a}");
        }
        _ => {
            assert!(false)
        }
    }
}

#[test]
fn test_char_parser_flat_map() {
    use crate::fp_parser::common_parser::char_parser;

    let p1 = char_parser('a');
    let p2 = p1.flat_map(move |c| char_parser('b').map(move |d| [c,d].iter().collect::<String>()));

    let input = "abcdefg";

    match p2.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(a, loc1) => {
            assert!(true);
            println!("{a}");
        }
        _ => {
            assert!(false)
        }
    }
}

#[test]
fn test_add() {
    use crate::fp_parser::common_parser::char_parser;
    let p = char_parser('a') + (char_parser('b'));
    let input = "abcdefg";

    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(a, loc1) => {
            let c1 = a.0;
            let c2 = a.1;
            assert!(true);
            println!("{c1}{c2}");
        }
        _ => {
            assert!(false)
        }
    }
}