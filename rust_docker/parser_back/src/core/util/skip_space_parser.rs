use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

use crate::core::util::skip_seq0_parser::skip_seq0_parser;
use crate::core::util::space_parser::space_parser;

pub fn skip_space_parser<'a>() -> Parser<'a, (), impl Fn(&'a [u8], usize) -> ParseResult<()> + Clone + 'a>
{
    skip_seq0_parser(space_parser())
}