use super::parser::*;
use super::parse_result::*;

pub struct OptionalParser<T> {parser: Box<Parser<T>>}
impl<T> OptionalParser<T> {
  pub fn new(parser: Box<Parser<T>>) -> Self {
    Self {parser: parser}
  }
}

impl <T>Parser<Option<T>> for OptionalParser<T> {
  fn parse(&self, input: String) -> ParseResult<Option<T>> {
    match self.parser.parse(input) {
      ParseResult::ParseOk{value: v, next: s} => ParseResult::ok(Some(v), s),
      ParseResult::ParseNg{message: m, next: s} => ParseResult::ok(None, s)
    }
  }
}

#[test]
fn test_lexer() {
  use super::word_parser::WordParser;
  let parser1 = OptionalParser::new(Box::new(WordParser::new("hello".to_string())));
  let result1 = parser1.parse("hello".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("hello, world".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), ", world".to_string());

  let result3 = parser1.parse("Hello".to_string());
  assert_eq!(result3.result(), true);
  assert_eq!(result3.next(), "Hello".to_string());
}