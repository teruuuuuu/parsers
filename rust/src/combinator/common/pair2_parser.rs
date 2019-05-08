use super::parser::*;
use super::parse_result::*;

struct Pair2Parser<T,U> {parser1: Box<Parser<T>>, parser2: Box<Parser<U>>}
impl<T,U> Pair2Parser<T,U> {
  pub fn new(parser1: Box<Parser<T>>, parser2: Box<Parser<U>>) -> Self {
    Self {parser1: parser1, parser2: parser2}
  }
}
impl <T,U>Parser<(T,U)> for Pair2Parser<T,U> {
  fn parse(&self, input: String) -> ParseResult<(T,U)> {
    match self.parser1.parse(input) {
      ParseResult::ParseOk{value: v, next: s} => {
        match self.parser2.parse(s) {
          ParseResult::ParseOk{value: w, next: t} => ParseResult::ok((v,w), t),
          ParseResult::ParseNg{message: m, next: t} => ParseResult::ng(m, t)
        }
      },
      ParseResult::ParseNg{message: m, next: s} => ParseResult::ng(m, s)
    }
  }
}

#[test]
fn test_lexer() {
  use super::many_parser::*;
  use super::word_parser::WordParser;

  let parser1 = Pair2Parser::new(
    Box::new(ManyParser::new(Box::new(WordParser::new("hello".to_string())))),
    Box::new(WordParser::new("world".to_string())));

  let result1 = parser1.parse("helloworld".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("hellohelloworld".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), "".to_string());

  let result3 = parser1.parse("hellohellohelloworld".to_string());
  assert_eq!(result3.result(), true);
  assert_eq!(result3.next(), "".to_string());

  let result4 = parser1.parse("world".to_string());
  assert_eq!(result4.result(), true);
  assert_eq!(result4.next(), "".to_string());

  let result5 = parser1.parse("World".to_string());
  assert_eq!(result5.result(), false);
  assert_eq!(result5.next(), "World".to_string());
}