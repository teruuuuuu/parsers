
use std::fmt::*;
use std::cmp;
use std::ops::Add;
use std::process::Output;


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
pub enum ParseResult<'a, T> {
    ParseOk(&'a T, Loc),
    ParseNg(String, Loc)
}

pub trait Parser<T> {
    fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<T>;
}

struct CParser {}
impl Parser<char> for CParser {
    fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<char> {
        ParseResult::ParseNg("()".to_owned(), Loc(0, 0))
    
    }
}

fn pchar(c: char) -> impl Parser<char> {
    CParser {}
}


#[test]
fn pchar_test() {
    let mut a = pchar('a');
    let b = a.parse("abc", Loc(0, 5));
    match b {
        ParseResult::ParseNg(m, l) => println!("parseng message:{m}"),
        _ => println!("")
    }
}

pub trait Parser2 {
    // type Output;

    fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<char>;
}

impl Parser2 {
    fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<char> {
        ParseResult::ParseNg("".to_string(), Loc(0, 0))
    }  
}

#[test]
fn test() {
    let a = vec!(1,2,5).into_boxed_slice();
}

fn double_positives<'a>(numbers: &'a Vec<i32>) -> impl Iterator<Item = i32> + 'a {
    numbers
        .iter()
        .filter(|x| x > &&0)
        .map(|x| x * 2)
}

// fn pchar(c: char) -> Box<dyn Parser2> {  

//     Box::new(Parser2 {
//         fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<char> {
//             ParseResult::ParseNg("".to_string(), Loc(0, 0))
//         }
//     })
    
//     // Box::new(impl Parser2 {
//     //     fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<char> {
//     //         ParseResult::ParseNg("".to_string(), Loc(0, 0))
//     //     }
//     // }
// }

//     fn parse(&mut self,input: &str, loc: Loc) -> ParseResult<Self::Output> {
//         let chars = input.chars();
//         let count = chars.count();
//         let index = loc.0;
//         if index >= count {
//             ParseResult::ParseNg { message: format!("input length over {} >= {}, ", index, count) }
//         } else {
//             match input.chars().nth(index) {
//                 Some(d) if c == d => ParseResult::ParseOk::<char> { result: c, loc: Loc(index, index + 1) },
//                 _ => ParseResult::ParseNg { message: format!("not {}: {}, ", c, &input[index..cmp::min(index+30, count)]) }
//             }    
//         }
//     }
// }
// }


// #[test]
// fn test_pchar() {
//     const INPUT: &str = "abcdefghi";
//     let pchar1 = pchar('a');
//     let pchar2 = pchar('c');

//     let result1 = pchar1.parse(INPUT, Loc(0, 3));
//     assert_eq!(result1, ParseResult::ParseOk::<char>('a', Loc(0,1)));

//     let result2 = pchar2(INPUT, Loc(1, 3));
//     match result2 {
//         ParseResult::ParseOk<char> { result:_, loc:_ } => assert!(false),
//         ParseResult::ParseNg<char> { message: _ } => assert!(true)
//     }

// }

// #[derive(Copy, Clone)]
// pub struct Map<P, F>(P, F);
// impl<A, B, P, F> Parser<Input> for Map<P, F>
// where
//     P: Parser<Input, Output = A>,
//     F: FnMut(A) -> B,
// {
//     type Output = B;
//     type PartialState = P::PartialState;

//     parse_mode!(Input);
//     #[inline]
//     fn parse_mode_impl<M>(
//         &mut self,
//         mode: M,
//         input: &mut Input,
//         state: &mut Self::PartialState,
//     ) -> ParseResult<Self::Output, <Input as StreamOnce>::Error>
//     where
//         M: ParseMode,
//     {
//         match self.0.parse_mode(mode, input, state) {
//             CommitOk(x) => CommitOk((self.1)(x)),
//             PeekOk(x) => PeekOk((self.1)(x)),
//             CommitErr(err) => CommitErr(err),
//             PeekErr(err) => PeekErr(err),
//         }
//     }

//     forward_parser!(Input, add_error add_committed_expected_error parser_count, 0);
// }

