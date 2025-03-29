use std::rc::Rc;

use crate::core::either::Either;
use crate::core::parser::{Parse, Parser};

use crate::prelude::ParserTrait;
use crate::util::item::and_parser::and_parser;
use crate::util::item::array_parser::array_parser;
use crate::util::item::bracket_parser::bracket_parser;
use crate::util::item::char_parser::char_parser;
use crate::util::item::dquote_string_parser::dquote_string_parser;
use crate::util::item::either_parser::either_parser;
use crate::util::item::end_parser::end_parser;
use crate::util::item::escape_parser::escape_parser;
use crate::util::item::float_parser::float_parser;
use crate::util::item::not_parser::not_parser;
use crate::util::item::number_parser::number_parser;
use crate::util::item::optional_parser::optional_parser;
use crate::util::item::or_parser::or_parser;
use crate::util::item::separate0_parser::separate0_parser;
use crate::util::item::separate1_parser::separate1_parser;
use crate::util::item::seq0_parser::seq0_parser;
use crate::util::item::seq1_parser::seq1_parser;
use crate::util::item::skip_parser::skip_parser;
use crate::util::item::skip_seq0_parser::skip_seq0_parser;
use crate::util::item::skip_space_parser::skip_space_parser;
use crate::util::item::space_parser::space_parser;
use crate::util::item::squote_string_parser::squote_string_parser;
use crate::util::item::stop_parser::stop_parser;
use crate::util::item::string_parser::string_parser;

pub struct ParserGen;

impl ParserGen {
    pub fn and<'a, A, B>(parser1: Parser<'a, A>, parser2: Parser<'a, B>) -> Parser<'a, (A, B)> 
    where 
        A: 'a + Clone,
        B: 'a + Clone,
    {
        and_parser(parser1, parser2)
    }

    pub fn array<'a, A, B, C>(parser: Parser<'a, A>, separate_parser: Parser<'a, B>, l_bracket_parser: Parser<'a, C>, r_bracket_parser: Parser<'a, C>) -> Parser<'a, Vec<A>>
    where 
        A: 'a + Clone,
        B: 'a + Clone,
        C: 'a + Clone,
    {
        array_parser(parser, separate_parser, l_bracket_parser, r_bracket_parser)
    }

    pub fn bracket<'a, A, B>(parser: Parser<'a, A>, l_bracket_parser: Parser<'a, B>, r_bracket_parser: Parser<'a, B>) -> Parser<'a, A>
    where
        A: Clone + 'a,
        B: Clone + 'a,
    {
        bracket_parser(parser, l_bracket_parser, r_bracket_parser)
    }

    pub fn char<'a>(c: char) -> Parser<'a, char> 
    {
        char_parser(c)
    }

    pub fn dquote_string<'a>() -> Parser<'a, String>
    {
        dquote_string_parser()
    }

    pub fn either<'a, A, B>(parser1: Parser<'a, A>, parser2: Parser<'a, B>) -> Parser<'a, Either<A, B>>
    where
        A: Clone + 'a,
        B: Clone + 'a
    {
        either_parser(parser1, parser2)
    }

    pub fn end<'a>() -> Parser<'a, ()>
    {
        end_parser()
    }

    pub fn escape<'a>(escape_char: char) -> Parser<'a, char> 
    {
        escape_parser(escape_char)
    }

    pub fn escape_squote<'a>() -> Parser<'a, char> {
        escape_parser('\'')
    }
    
    pub fn escape_dquote<'a>() -> Parser<'a, char> {
        escape_parser('"')
    }
    

    pub fn float<'a>() -> Parser<'a, f64>
    {
        float_parser()
    }

    pub fn from_parse_func<'a, A>(parse_func: Rc<Parse<'a, A>>) -> Parser<'a, A> 
    where 
        A: Clone + 'a
    {
        Parser::new(parse_func)
    }

    pub fn not<'a, A>(parser: Parser<'a, A>) -> Parser<'a, char> 
    where 
        A: Clone + 'a
    {
        not_parser(parser)
    }

    pub fn number<'a>() -> Parser<'a, i64>
    {
        number_parser()
    }

    pub fn optional<'a, A>(parser: Parser<'a, A>) -> Parser<'a, Option<A>>
    where
        A: Clone + 'a,
    {
        optional_parser(parser)
    }

    pub fn or<'a, A>(parser1: Parser<'a, A>, parser2: Parser<'a, A>) -> Parser<'a, A>
    where
        A: Clone + 'a,
    {
        or_parser(parser1, parser2)
    }

    pub fn separate0<'a, A, B>(parser: Parser<'a, A>, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        A: Clone + 'a,
        B: Clone + 'a,
    {
        separate0_parser(parser, separate_parser)
    }

    pub fn separate1<'a, A, B>(parser: Parser<'a, A>, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        A: Clone + 'a,
        B: Clone + 'a,
    {
        separate1_parser(parser, separate_parser)
    }

    pub fn seq0<'a, A>(parser: Parser<'a, A>) -> Parser<'a, Vec<A>>
    where
        A: Clone + 'a
    {
        seq0_parser(parser)
    }

    pub fn seq1<'a, A>(parser: Parser<'a, A>) -> Parser<'a, Vec<A>>
    where
        A: Clone + 'a
    {
        seq1_parser(parser)
    }

    pub fn skip<'a, A>(parser: Parser<'a, A>) -> Parser<'a, ()>
    where
        A: Clone + 'a,
    {
        skip_parser(parser)
    }

    pub fn skip_seq0<'a, A>(parser: Parser<'a, A>) -> Parser<'a, ()>
    where
        A: Clone + 'a,
    {
        skip_seq0_parser(parser)
    }

    pub fn skip_space<'a>() -> Parser<'a, ()>
    {
        skip_space_parser()
    }

    pub fn space<'a>() -> Parser<'a, char>
    {
        space_parser()
    }

    pub fn squote_string<'a>() -> Parser<'a, String>
    {
        squote_string_parser()
    }

    pub fn stop<'a, A, F>(parser: Parser<'a, A>) -> Parser<'a, String> 
    where 
        A: Clone + 'a,
    {
        stop_parser(parser)
    }

    pub fn str<'a>(str: String) -> Parser<'a, String>
    {
        string_parser(str)
    }
}