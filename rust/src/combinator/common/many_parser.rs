use super::parser::*;
use super::parse_result::*;

pub struct ManyParser<T> {parser: Box<Parser<T>>}
impl<T> ManyParser<T> {
  pub fn new(parser: Box<Parser<T>>) -> Self {
    Self {parser: parser}
  }
}
impl <T>Parser<Vec<T>> for ManyParser<T> {
  fn parse(&self, input: String) -> ParseResult<Vec<T>> {
    let mut values = Vec::new();
    let mut next = input.to_string();
    loop {
      match self.parser.parse(next.to_string()) {
        ParseResult::ParseOk{value: v, next: s} => {
          values.push(v);
          next = s;
        },
        ParseResult::ParseNg{..} => {
          break;
        }
      }
    }
    ParseResult::ok(values, next)
  }
}

#[test]
fn test_lexer() {
  use super::word_parser::WordParser;
  
  let parser1 = ManyParser::new(
    Box::new(WordParser::new("hello".to_string())));

  let result1 = parser1.parse("hello".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("hello, world".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), ", world".to_string());

  let result3 = parser1.parse("hellohellohello, world".to_string());
  assert_eq!(result3.result(), true);
  assert_eq!(result3.next(), ", world".to_string());
  
  let result4 = parser1.parse("Hello, world".to_string());
  assert_eq!(result4.result(), true);
  assert_eq!(result4.next(), "Hello, world".to_string());
}