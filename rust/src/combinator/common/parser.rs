use super::parse_result::ParseResult;

pub trait Parser<T> {
    fn parse(&self, str: String) -> ParseResult<T>;
}
