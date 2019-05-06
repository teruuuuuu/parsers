#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseResult<T> {
 ParseOk {value: T, next: String},
 ParseNg{message: String, next: String}
}

impl<T> ParseResult<T> {
  pub fn ng(message: String, next: String) -> Self {
    ParseResult::ParseNg{message: message, next: next}
  }
  pub fn ok(value: T, next: String) -> Self {
    ParseResult::ParseOk{value: value, next: next}
  }
  pub fn result(&self) -> bool {
    match self {
      ParseResult::ParseOk{..} => true,
      ParseResult::ParseNg{..} => false,
    }
  }
  pub fn next(&self) -> String {
    match self {
      ParseResult::ParseOk{next, ..} => next.to_string(),
      ParseResult::ParseNg{next, ..} => next.to_string(),
    }
  }
}