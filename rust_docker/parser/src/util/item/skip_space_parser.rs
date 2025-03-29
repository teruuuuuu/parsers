use crate::core::parser::Parser;
// use crate::core::parse_result::ParseResult;

use crate::util::item::skip_seq0_parser::skip_seq0_parser;
use crate::util::item::space_parser::space_parser;

pub fn skip_space_parser<'a>() -> Parser<'a, ()>
{
    skip_seq0_parser(space_parser())
}