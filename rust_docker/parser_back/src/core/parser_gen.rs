use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

use crate::core::util::char_parser::char_parser;
use crate::core::util::dquote_string_parser::dquote_string_parser;
use crate::core::util::end_parser::end_parser;
use crate::core::util::float_parser::float_parser;
use crate::core::util::number_parser::number_parser;
use crate::core::util::space_parser::space_parser;
use crate::core::util::squote_string_parser::squote_string_parser;
use crate::core::util::string_parser::string_parser;

pub struct ParserGen;

impl ParserGen {
    pub fn char<'a>(c: char) -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a> 
    {
        char_parser(c)
    }

    pub fn dqoute_string_parser<'a>() -> Parser<'a, String, impl Fn(&'a [u8], usize) -> ParseResult<String> + Clone + 'a>
    {
        dquote_string_parser()
    }

    pub fn end<'a>() -> Parser<'a, (), impl Fn(&'a [u8], usize) -> ParseResult<()> + Clone + 'a>
    {
        end_parser()
    }

    pub fn float_parser<'a>() -> Parser<'a, f64, impl Fn(&'a [u8], usize) -> ParseResult<f64> + Clone + 'a> 
    {
        float_parser()
    }

    pub fn number<'a>() -> Parser<'a, i64, impl Fn(&'a [u8], usize) -> ParseResult<i64> + Clone + 'a>
    {
        number_parser()
    }

    pub fn space<'a>() -> Parser<'a, char, impl Fn(&'a [u8], usize) -> ParseResult<char> + Clone + 'a>
    {
        space_parser()
    }

    pub fn sqoute_string_parser<'a>() -> Parser<'a, String, impl Fn(&'a [u8], usize) -> ParseResult<String> + Clone + 'a>
    {
        squote_string_parser()
    }

    pub fn string<'a>(str: String) -> Parser<'a, String, impl Fn(&'a [u8], usize) -> ParseResult<String> + Clone + 'a>
    {
        string_parser(str)
    }

}
