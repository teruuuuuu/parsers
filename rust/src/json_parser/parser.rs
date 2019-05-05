use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::Peekable as Peekable;
use super::lex::*; 

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
struct JString {v: String}
impl JString {pub fn new(v: String ) -> Self {Self {v}}}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct JNumber {v: i64}
impl JNumber {pub fn new(v: i64 ) -> Self {Self {v}}}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JObject {v: HashMap<JString, Ast> }
impl JObject {pub fn new(v: HashMap<JString, Ast>) -> Self {Self {v}}}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JArray {v: Vec<Ast> }
impl JArray {pub fn new(v: Vec<Ast> ) -> Self {Self {v}}}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JBool {v: bool }
impl JBool {pub fn new(v: bool) -> Self {Self {v}}}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JNull {}
impl JNull {pub fn new() -> Self {Self {}}}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AstKind {
  Object(JObject),
  Array(JArray),
  String(JString),
  Number(JNumber),
  Bool(JBool),
  Null(JNull),
}

pub type Ast = Annot<AstKind>;
impl Ast {
  pub fn object(n: JObject, loc: Loc) -> Self {
    Self::new(AstKind::Object(n), loc)
  }
  pub fn array(n: JArray, loc: Loc) -> Self {
    Self::new(AstKind::Array(n), loc)
  }
  pub fn string(n: String, loc: Loc) -> Self {
    Self::new(AstKind::String(JString::new(n)), loc)
  }
  pub fn number(n: i64, loc: Loc) -> Self {
    Self::new(AstKind::Number(JNumber::new(n)), loc)
  }
  pub fn bool(n: bool, loc: Loc) -> Self {
    Self::new(AstKind::Bool(JBool::new(n)), loc)
  }
  pub fn null(loc: Loc) -> Self {
    Self::new(AstKind::Null(JNull::new()), loc)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
  Error,
  UnexpectedToken(Token),
  NotExpression(Token),
  NotOperator(Token),
  UnclosedOpenParen(Token),
  RedundantExpression(Token),
  Eof,
}

fn parse_value<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
  match tokens.peek().map(|tok| tok.clone()) {
    Some(Token {value:TokenKind::String(x), loc: loc}) => {
      tokens.next();
      Ok(Ast::string(x, loc))
    }
    Some(Token {value:TokenKind::Number(x), loc: loc}) => {
      tokens.next();
      Ok(Ast::number(x, loc))
    }
    Some(Token {value:TokenKind::Bool(x), loc: loc}) => {
      tokens.next();
      Ok(Ast::bool(x, loc))
    }
    Some(Token {value:TokenKind::Null, loc: loc}) => {
      tokens.next();
      Ok(Ast::null(loc))
    }
    _ => {
      parse_element(tokens)
    }
  }
}

fn parse_element<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
  match parse_array(tokens) {
    Ok(x) => Ok(x),
    Err(_) => parse_object(tokens),
  }
}

fn parse_object<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
  fn parse_keyvalue<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<(Option<(JString, Ast)>, Option<Loc>), ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
      match tokens.next() {
        Some(Token {value: TokenKind::String(x), ..}) => {
          match tokens.next() {
            Some(Token {value: TokenKind::Colon, ..}) => {
              match parse_value(tokens) {
                Ok(y) => Ok((Some((JString::new(x), y)), None)),
                _ => Err(ParseError::Error),
              }
            },
            _ => Err(ParseError::Error),
          }
        },
        Some(Token {value: TokenKind::RParen, loc: loc}) => Ok((None, Some(loc))),
        _ => Err(ParseError::Error),
      }
    }

  let mut map: HashMap<JString, Ast> = HashMap::new();
  match tokens.peek().map(|tok| tok.clone()) {
    Some(Token {value: TokenKind::LParen, loc: Loc(lstart, _)}) => {
      tokens.next();
        match parse_keyvalue(tokens) {
          Ok((Some((k, v)), None)) => {
            map.insert(k, v);
            loop {
              match tokens.next() {
                Some(Token {value: TokenKind::RParen, loc: Loc(_, lend)}) => {
                  return Ok(Ast::object(JObject::new(map), Loc(lstart, lend)));
                },
                Some(Token {value: TokenKind::Comma, ..}) => {
                  match parse_keyvalue(tokens) {
                    Ok((Some((k, v)), None)) => {
                      map.insert(k, v);
                    },
                    _ => {
                      return Err(ParseError::Error);
                    }
                  }
                },
                _ => {
                  return Err(ParseError::Error);
                }
              }
            }
          },
          Ok((None, Some(Loc(_, lend)))) => Ok(Ast::object(JObject::new(map), Loc(lstart, lend))),
          _ => Err(ParseError::Error)
        }
      },
    _ => Err(ParseError::Error),
  }
}


fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
  let mut vec = Vec::new();
  match tokens.peek().map(|tok| tok.clone()) {
    Some(Token {value: TokenKind::LBracket, loc: Loc(lstart, _)}) => {
      tokens.next();
      match parse_value(tokens) {
        Ok(x) => {
          vec.push(x);
          loop {
            match tokens.peek() {
              Some(Token {value: TokenKind::Comma, .. }) => {
                tokens.next();
                match parse_value(tokens) {
                  Ok(x) => {
                    vec.push(x);
                  },
                  _ => {
                    return Err(ParseError::Error);
                  }
                };
              },
              Some(Token {value: TokenKind::RBracket, loc: Loc(_, lend)}) => {
                return Ok(Ast::array(JArray::new(vec), Loc(lstart, *lend)));
              },
              _ => {
                return Err(ParseError::Error);
              }
            }
          }
        },
        _ => {
          match tokens.next() {
            Some(Token {value: TokenKind::RBracket, loc: Loc(_, lend)}) => {
              Ok(Ast::array(JArray::new(vec), Loc(lstart, lend)))
            },
            _ => Err(ParseError::Error)
          }
        }
      }
    },
    _ => Err(ParseError::Error)
  }
}



fn parse<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
  parse_element(tokens)
}


pub fn main() {
  let mut tokens = &mut vec![
      Token::lparen(Loc(0, 1)),
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
      Token::lparen(Loc(21, 22)),
      Token::rparen(Loc(22, 23)),
      Token::rbracket(Loc(24, 25)),
      Token::rparen(Loc(25, 26))
    ].into_iter().peekable();

  println!("{:?}", parse(tokens));
}