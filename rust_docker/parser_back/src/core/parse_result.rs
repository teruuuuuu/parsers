use crate::core::parse_error::ParseError;

#[derive(Clone, PartialEq, Debug)]
pub enum ParseResult<A> {
    Success { value: A, location: usize },
    Failure { parse_error: ParseError, location: usize },
}

impl<A> ParseResult<A> {
    pub fn successful(value: A, location: usize) -> Self {
        ParseResult::Success { value, location }
    }

    pub fn failure(parse_error: ParseError, location: usize) -> Self {
        ParseResult::Failure { parse_error, location }
    }

    pub fn map<B, F>(self, f: F) -> ParseResult<B>
    where
        F: Fn(A) -> B,
    {
        match self {
            ParseResult::Success { value, location } =>
                ParseResult::successful(f(value), location),
            ParseResult::Failure { parse_error, location } => {
                ParseResult::Failure { parse_error, location }
            }
        }
    }
}
