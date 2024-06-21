use std::fmt::*;
use std::cmp;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
  pub fn merge(&self, other: &Loc) -> Loc {
    use::std::cmp::{max, min};
    Loc(min(self.0, other.0), max(self.1, other.1))
  }
}

#[derive(Clone, Debug, PartialEq)]
enum ParseResult<T> {
    Success{result: T, loc: Loc},
    Fail{message: String},
} 

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Loc({}, {})", self.0, self.1)
    }
}

impl <'a, T>std::fmt::Display for ParseResult<T> 
where T: Copy + std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseResult::Success{result, loc} => write!(f, "ParseSuccess({}, {})", result, loc),
            ParseResult::Fail{message} => write!(f, "PrseFail({})", message)
        }
    }
}

#[test]
fn test_parse_result() {
    println!("{}", ParseResult::Success::<u32> { result: 123, loc: Loc(0, 1) });
    println!("{}", ParseResult::Fail::<i32> { message: format!("ng message") });
}

type Parser<T> = dyn for<'a> Fn(&'a str, usize) -> ParseResult<T>;
// type Parser<T> = impl Fn(&str, usize) -> ParseResult<T>;



// impl FnはBox<dyn Fnと似たようなものだがライフタイムの
fn pchar(c: char) -> impl Fn(&str, usize) -> ParseResult<char> {
    move |input: &str, index: usize| {
        let chars = input.chars();
        let count = chars.count();
        if index >= count {
            ParseResult::Fail { message: format!("input length over {} >= {}, ", index, count) }
        } else {
            match input.chars().nth(index) {
                Some(d) if c == d => ParseResult::Success::<char> { result: c, loc: Loc(index, index + 1) },
                _ => ParseResult::Fail { message: format!("not {}: {}, ", c, &input[index..cmp::min(index+30, count)]) }
            }    
        }
    }
}

#[test]
fn test_pchar() {
    const INPUT: &str = "abcdefghi";
    let pchar1 = pchar('a');
    let pchar2 = pchar('c');

    let result1 = pchar1(INPUT, 0);
    assert_eq!(result1, ParseResult::Success { result: 'a', loc: Loc(0,1) });

    let result2 = pchar2(INPUT, 1);
    match result2 {
        ParseResult::Success { result:_, loc:_ } => assert!(false),
        ParseResult::Fail { message: _ } => assert!(true)
    }
}


trait Functor {
    fn fmap<A,B>(f: fn(A) -> B, parser: Parser<A>) -> Parser<B>;
}
