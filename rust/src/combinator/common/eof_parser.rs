use super::parser::*;
use super::parse_result::*;

pub struct EofParser {}
impl EofParser {
  pub fn new() -> Self {
    Self {}
  }
}

impl Parser<()> for EofParser {
  fn parse(&self, input: String) -> ParseResult<()> {
    let u8_bytes = input.as_bytes();
    let mut pos = 0 as usize;
    while pos < u8_bytes.len() && b" \n\t".contains(&u8_bytes[pos]){
      pos += 1;
    }
    if pos == u8_bytes.len() {
      ParseResult::ok((), "".to_string())
    } else {
      ParseResult::ng("not eof".to_string(), input.to_string())
    }
  }
}

#[test]
fn test_lexer() {
  let parser1 = EofParser::new();
  let result1 = parser1.parse("".to_string());
  assert_eq!(result1.result(), true);
  assert_eq!(result1.next(), "".to_string());

  let result2 = parser1.parse("  ".to_string());
  assert_eq!(result2.result(), true);
  assert_eq!(result2.next(), "".to_string());

  let result3 = parser1.parse("  h".to_string());
  assert_eq!(result3.result(), false);
  assert_eq!(result3.next(), "  h".to_string());

  let result4 = parser1.parse("  
  ".to_string());
  assert_eq!(result4.result(), true);
  assert_eq!(result4.next(), "".to_string());
  println!("{:?}", result4);
}