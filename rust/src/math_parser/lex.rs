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
  Plus,
  Minus,
  Asterisk,
  Slash,
  LParen,
  RParen,
}
pub type Token = Annot<TokenKind>;

impl Token {
  pub fn number(n: u64, loc: Loc) -> Self {
    Self::new(TokenKind::Number(n), loc)
  }
  pub fn plus(loc: Loc) -> Self {
    Self::new(TokenKind::Plus, loc)
  }
  pub fn minus(loc: Loc) -> Self {
    Self::new(TokenKind::Minus, loc)
  }
  pub fn asterisk(loc: Loc) -> Self {
    Self::new(TokenKind::Asterisk, loc)
  }
  pub fn slash(loc: Loc) -> Self {
    Self::new(TokenKind::Slash, loc)
  }
  pub fn lparen(loc: Loc) -> Self {
    Self::new(TokenKind::LParen, loc)
  }
  pub fn rparen(loc: Loc) -> Self {
    Self::new(TokenKind::RParen, loc)
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

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
  let mut tokens = Vec::new();
  let input = input.as_bytes();
  let mut pos = 0;
  macro_rules! lex_a_token {
    ($lexer:expr) => {{
      let (tok, p) = $lexer?;
      tokens.push(tok);
      pos = p;
    }};
  }
  while pos < input.len() {
    match input[pos] {
      b'0'...b'9' => lex_a_token!(lex_number(input, pos)),
      b'+' => lex_a_token!(lex_plus(input, pos)),
      b'-' => lex_a_token!(lex_minus(input, pos)),
      b'*' => lex_a_token!(lex_asterisk(input, pos)),
      b'/' => lex_a_token!(lex_slash(input, pos)),
      b'(' => lex_a_token!(lex_lparen(input, pos)),
      b')' => lex_a_token!(lex_rparen(input, pos)),
      b' ' | b'\n' | b'\t'  => {
        let ((), p) = skip_space(input, pos);
        pos = p;
      }
      b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1 ))),
    }
  }
  Ok(tokens)
}

fn lex_number(input: &[u8], mut pos: usize) -> Result<(Token, usize), LexError> {
  use std::str::from_utf8;

  let start = pos;
  while pos < input.len() && b"1234567890".contains(&input[pos]) {
    pos += 1;
  }
  let n = from_utf8(&input[start..pos]).unwrap().parse().unwrap();
  Ok((Token::number(n, Loc(start, pos)), pos))
}

fn skip_space(input: &[u8], mut pos: usize) -> ((), usize) {
  while pos < input.len() && b" \n\t".contains(&input[pos]){
    pos += 1;
  }
  ((), pos)
}

fn lex_plus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b'+').map(|(_, end)| (Token::plus(Loc(start, end)), end))
}
fn lex_minus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b'-').map(|(_, end)| (Token::minus(Loc(start, end)), end))
}
fn lex_asterisk(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b'*').map(|(_, end)| (Token::asterisk(Loc(start, end)), end))
}
fn lex_slash(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b'/').map(|(_, end)| (Token::slash(Loc(start, end)), end))
}
fn lex_lparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b'(').map(|(_, end)| (Token::lparen(Loc(start, end)), end))
}
fn lex_rparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
  consume_byte(input, start, b')').map(|(_, end)| (Token::rparen(Loc(start, end)), end))
}

fn consume_byte(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
  if input.len() <= pos {
    return Err(LexError::eof(Loc(pos, pos)));
  }
  if input[pos] != b {
    return Err(LexError::invalid_char(
      input[pos] as char,
      Loc(pos, pos + 1),
    ));
  }
  Ok((b, pos + 1))
}

#[test]
fn test_lexer() {
  assert_eq!(
    lex("1 + 2 * 3 - -10"),
    Ok(vec![
      Token::number(1, Loc(0, 1)),
      Token::plus(Loc(2, 3)),
      Token::number(2, Loc(4, 5)),
      Token::asterisk(Loc(6, 7)),
      Token::number(3, Loc(8, 9)),
      Token::minus(Loc(10, 11)),
      Token::minus(Loc(12, 13)),
      Token::number(10, Loc(13, 15)),
    ])
  )
}