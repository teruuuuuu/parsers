use super::parser::*;
use super::parse_result::*;

pub struct StringParser {str: String}
impl StringParser {
  pub fn new(str: String) -> Self {
    Self {str: str}
  }
}

impl Parser<String> for StringParser {
  fn parse(&self, input: String) -> ParseResult<String> {
    if input.len() >= self.str.len() && input[..self.str.len()] == self.str {
      ParseResult::ok(self.str.to_string(), input[self.str.len()..].to_string())
    } else {
      ParseResult::ng(format!("not match: {}", self.str.to_string()), input.to_string())
    }
  }
}

#[test]
fn test_lexer() {
  let parser1 = StringParser::new("hello".to_string());
  let result1 = parser1.parse("hello".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("hello, world".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), ", world".to_string());

  let result3 = parser1.parse("Hello".to_string());
  assert_eq!(result3.result(), false);
  assert_eq!(result3.next(), "Hello".to_string());
}