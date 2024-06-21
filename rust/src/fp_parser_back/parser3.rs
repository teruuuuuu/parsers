
use std::fmt::*;
// use std::cmp;
// use std::ops::Add;
// use std::process::Output;
// use std::str;
use std::rc::Rc;

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
pub enum ParseResult<T> {
    ParseOk(T, Loc),
    ParseNg(String, Loc)
}

type Parse<'a, T> = dyn Fn(&'a str, Loc) -> ParseResult<T> + 'a;

struct Parser<'a, T> {
    pub parse: Rc<Parse<'a, T>>
}

impl <'a, T>Parser<'a, T> {
    pub fn new<F>(parse: F) -> Parser<'a, T>
    where
      F: Fn(&'a str, Loc) -> ParseResult<T> + 'a {Parser { parse: Rc::new(parse)}}
}

impl <'a, T>Clone for Parser<'a, T> {
    fn clone(&self) -> Self {
        Self {
            parse: self.parse.clone(),
        }
    }
}

trait ParserTrait<'a, T> {
    fn parse(&self,input: &'a str, loc: Loc ) -> ParseResult<T>;
}

impl <'a, T>ParserTrait<'a, T> for Parser<'a, T> {
    fn parse(&self,input: &'a str, loc: Loc ) -> ParseResult<T> {
        (self.parse)(input, loc)
    }
}

trait FunctorParser<'a, T>: ParserTrait<'a, T> {
    fn map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(T) -> B + 'a,
      T: 'a;
}

impl <'a, T>FunctorParser<'a, T> for Parser<'a, T> {
    fn map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(T) -> B + 'a,
      T: 'a {
        Parser::new(move |input: &str, loc: Loc| match self.parse(input, loc) {
            ParseResult::ParseOk(r, loc) => ParseResult::ParseOk(f(r), loc),
            ParseResult::ParseNg(m, loc) => ParseResult::ParseNg(m, loc)
        })
      }
}

trait MonadParser<'a, T>: FunctorParser<'a, T> {
    fn flat_map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(T) -> Parser<'a, B> + 'a,
      T: 'a;
}

impl <'a, T>MonadParser<'a, T> for Parser<'a, T> {
    fn flat_map<B, F>(self, f: F) -> Parser<'a, B>
    where
      F: Fn(T) -> Parser<'a, B> + 'a,
      T: 'a {
        Parser::new(move |input: &str, loc: Loc| match self.parse(input, loc) {
            ParseResult::ParseOk(r, loc) => f(r).parse(input, loc),
            ParseResult::ParseNg(m, loc) => ParseResult::ParseNg(m, loc)
        })
      }
}

fn char_parser<'a>(c: char) -> Parser<'a, char> {
    Parser::new(move |input: &str, loc: Loc| {
        let chars = input.chars();
        let count = chars.count();
        let index = loc.0;
        if index >= count {
            ParseResult::ParseNg(format!("index invalid "), loc)
        } else {
            match input.chars().nth(index) {
                Some(d) if c == d => ParseResult::ParseOk::<char>(c, Loc(index+1, loc.1)),
                _ => ParseResult::ParseNg(format!("not match "), loc)
            }    
        }
    })
}

#[test]
fn test_char_parser() {

    let p1 = char_parser('a');
    let p2 = char_parser('b');
    let p3 = char_parser('c');

    let input = "abcdefg";

    match p1.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(a, loc1) => {
            assert!(true);
            println!("{a}");
            match p2.parse(input, loc1) {
                ParseResult::ParseOk(b, loc2) => {
                    assert!(true);
                    println!("{b}");
                    match p3.parse(input, loc2) {
                        ParseResult::ParseOk(c, loc3) => {
                            assert!(true);
                            println!("{c}");
                            match p1.parse(input, loc3) {
                                ParseResult::ParseNg(message, _) => {
                                    assert!(true);
                                    println!("{message}");
                                },
                                _ => assert!(false)
                            }
                        }
                        _ => assert!(false)
                    }

                },
                _ => assert!(false)
            }

        },
        _ => {
            assert!(false)
        }
    }
}

#[test]
fn test_char_parser_map() {
    let p1 = char_parser('a');
    let p2 = p1.map(|c:char| String::from(c));

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