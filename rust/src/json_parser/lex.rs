use std::str::from_utf8;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
  pub fn merge(&self, other: &Loc) -> Loc {
    use::std::cmp::{max, min};
    Loc(min(self.0, other.0), max(self.1, other.1))
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
  pub value: T,
  pub loc: Loc,
}
impl<T> Annot<T> {
  pub fn new(value: T, loc: Loc) -> Self {
    Self {value, loc}
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
  Number(u64),
  String(String),
  Bool(bool),
  Comma, // ,
  Colon, // :
  LParen, // {
  RParen, // }
  LBracket, // [
  RBracket, // ]
}

pub type Token = Annot<TokenKind>;

impl Token {
  pub fn number(n: u64, loc: Loc) -> Self {
    Self::new(TokenKind::Number(n), loc)
  }
  pub fn string(v: String, loc: Loc) -> Self {
    Self::new(TokenKind::String(v), loc)
  }
  pub fn bool(b: bool, loc: Loc) -> Self {
    Self::new(TokenKind::Bool(b), loc)
  }
  pub fn comma(loc: Loc) -> Self {
    Self::new(TokenKind::Comma, loc)
  }
  pub fn collon(loc: Loc) -> Self {
    Self::new(TokenKind::Colon, loc)
  }
  pub fn lParen(loc: Loc) -> Self {
    Self::new(TokenKind::LParen, loc)
  }
  pub fn rparen(loc: Loc) -> Self {
    Self::new(TokenKind::RParen, loc)
  }
  pub fn lbracket(loc: Loc) -> Self {
    Self::new(TokenKind::LBracket, loc)
  }
  pub fn rbracket(loc: Loc) -> Self {
    Self::new(TokenKind::RBracket, loc)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
  InvalidChar(char),
  Eof,
}

pub type LexError = Annot<LexErrorKind>;
impl LexError {
  fn invalid_char(c: char, loc: Loc) -> Self {
    LexError::new(LexErrorKind::InvalidChar(c), loc)
  }
  fn eof(loc: Loc) -> Self {
    LexError::new(LexErrorKind::Eof, loc)
  }
}

pub struct Lexer;
impl Lexer {
  pub fn new() -> Self {
     Lexer
  }
  pub fn lex(&self, input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let input = input.as_bytes();
    let mut pos = 0;
    macro_rules! lex_a_token {
      ($lexer:expr) => {{
        let (tok, p) = $lexer;
        tokens.push(tok);
        pos = p;
      }};
    }
    let word_funcs = [self::lex_str, self::lex_number, self::lex_true, self::lex_false, self::lex_colon, self::lex_comma, 
      self::lex_lparen, self::lex_rparen, self::lex_lbracket, self::lex_rbracket];
    while pos < input.len() {

      let result =  word_funcs.into_iter().map(|f| f(input, pos))
        .filter_map(|v|  v.ok()).collect::<Vec<_>>();

      if result.len() > 1 {
        return Err(LexError::invalid_char(input[pos] as char, Loc(pos, pos + 1 )))
      } else if result.len() == 1 {
        lex_a_token!(result[0].clone());
      } else {
        if b" \n\t".contains(&input[pos]) {
          let ((), p) = skip_space(input, pos);
          pos = p;
        } else {
          return Err(LexError::invalid_char(input[pos] as char, Loc(pos, pos + 1 )))
        }
      }
    }
    Ok(tokens)
  }
}


fn lex_str(input: &[u8], mut pos: usize) -> Result<(Token, usize), LexError> {
  let start = pos;
  
  pos += 1;
  while pos < input.len() && input[pos] != b'"' {
    pos += 1;
  }
  let end = pos + 1;
  if input[start] != b'"' || input[end - 1] != b'"' {
    return Err(
      LexError::invalid_char('"',  Loc(start, end),
    ));
  }
  let s = from_utf8(&input[start..end]).unwrap().parse().unwrap();
  Ok((Token::string(s, Loc(start, end)), end))
}

fn lex_number(input: &[u8], mut pos: usize) -> Result<(Token, usize), LexError> {
  use std::str::from_utf8;

  let start = pos;
  while pos < input.len() && b"1234567890".contains(&input[pos]) {
    pos += 1;
  }
  if start == pos {
    return Err(
      LexError::invalid_char(input[start] as char,  Loc(start, start+1),
    ));
  }
  let n = from_utf8(&input[start..pos]).unwrap().parse().unwrap();
  Ok((Token::number(n, Loc(start, pos)), pos))
}

fn lex_true(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"true").map(|(_, end)| (Token::bool(true, Loc(start, end)), end))
}

fn lex_false(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"false").map(|(_, end)| (Token::bool(false, Loc(start, end)), end))
}

fn lex_comma(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b",").map(|(_, end)| (Token::comma(Loc(start, end)), end))
}

fn lex_colon(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b":").map(|(_, end)| (Token::collon(Loc(start, end)), end))
}

fn lex_lparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"{").map(|(_, end)| (Token::lParen(Loc(start, end)), end))
}

fn lex_rparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"}").map(|(_, end)| (Token::rparen(Loc(start, end)), end))
}

fn lex_lbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"[").map(|(_, end)| (Token::lbracket(Loc(start, end)), end))
}

fn lex_rbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_bytes(input, start, b"]").map(|(_, end)| (Token::rbracket(Loc(start, end)), end))
}

fn skip_space(input: &[u8], mut pos: usize) -> ((), usize) {
  while pos < input.len() && b" \n\t".contains(&input[pos]){
    pos += 1;
  }
  ((), pos)
}

fn consume_bytes(input: &[u8], pos: usize, bs: &[u8]) -> Result<(String, usize), LexError> {
  if input.len() < pos + bs.len() {
    return Err(LexError::eof(Loc(pos, pos)));
  }
  
  if from_utf8(&input[pos..pos+bs.len()]).unwrap() != from_utf8(bs).unwrap() {
    return Err(LexError::invalid_char(
      input[pos] as char,
      Loc(pos, pos + bs.len()),
    ));
  }
  Ok((from_utf8(bs).unwrap().parse().unwrap(), pos + bs.len()))
}

#[test]
fn test_lexer() {
  let mut lexer = Lexer::new();
  
  assert_eq!(
    lexer.lex("{\"a\": [ [1, 2, 3,4], {} ]}"),
    Ok(vec![
      Token::lParen(Loc(0, 1)),
      Token::string("\"a\"".to_string(), Loc(1, 4)),
      Token::collon(Loc(4, 5)),
      Token::lbracket(Loc(6, 7)),
      Token::lbracket(Loc(8, 9)),
      Token::number(1, Loc(9, 10)),
      Token::comma(Loc(10, 11)),
      Token::number(2, Loc(12, 13)),
      Token::comma(Loc(13, 14)),
      Token::number(3, Loc(15, 16)),
      Token::comma(Loc(16, 17)),
      Token::number(4, Loc(17, 18)),
      Token::rbracket(Loc(18, 19)),
      Token::comma(Loc(19, 20)),
      Token::lParen(Loc(21, 22)),
      Token::rparen(Loc(22, 23)),
      Token::rbracket(Loc(24, 25)),
      Token::rparen(Loc(25, 26))
    ])
  )
}