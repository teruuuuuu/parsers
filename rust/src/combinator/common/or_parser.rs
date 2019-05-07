use super::parser::*;
use super::parse_result::*;

pub struct OrParser<T> {parser1: Box<Parser<T>>, parser2: Box<Parser<T>>}
impl<T> OrParser<T> {
  pub fn new(parser1: Box<Parser<T>>, parser2: Box<Parser<T>>) -> Self {
    Self {parser1: parser1, parser2: parser2}
  }
}
impl <T>Parser<T> for OrParser<T> {
  fn parse(&self, input: String) -> ParseResult<T> {
    match self.parser1.parse(input.to_string()) {
      ParseResult::ParseOk{value: v, next: s} => ParseResult::ok(v, s),
      ParseResult::ParseNg{..} => {
        match self.parser2.parse(input.to_string()) {
          ParseResult::ParseOk{value: v, next: t} => ParseResult::ok(v, t),
          ParseResult::ParseNg{message: m, next: t} => ParseResult::ng(m, t)
        }
      }
    }
  }
}

#[test]
fn test_lexer() {
  use super::string_parser::*;
  let parser1 = OrParser::new(
    Box::new(StringParser::new("hello".to_string())), Box::new(StringParser::new("world".to_string())));

  let result1 = parser1.parse("hello".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("world".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), "".to_string());
  
  let result3 = parser1.parse("Hello, world".to_string());
  assert_eq!(result3.result(), false);
  assert_eq!(result3.next(), "Hello, world".to_string());
}