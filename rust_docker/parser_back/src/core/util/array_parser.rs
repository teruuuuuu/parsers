use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

use crate::core::util::bracket_parser::bracket_parser;
use crate::core::util::separate0_parser::separate0_parser;

pub fn array_parser<'a, A, F, B, G, C, H>(parser: Parser<'a, A, F>, separate_parser: Parser<'a, B, G>, l_bracket_parser: Parser<'a, C, H>, r_bracket_parser: Parser<'a, C, H>) -> Parser<'a, Vec<A>, impl Fn(&'a [u8], usize) -> ParseResult<Vec<A>> + Clone + 'a>
where
    A: Clone + 'a,
    F: Fn(&'a [u8], usize) -> ParseResult<A> + 'a,
    B: Clone + 'a,
    G: Fn(&'a [u8], usize) -> ParseResult<B> + 'a,
    C: Clone + 'a,
    H: Fn(&'a [u8], usize) -> ParseResult<C> + 'a,
{
    bracket_parser(separate0_parser(parser, separate_parser), l_bracket_parser, r_bracket_parser)
}


#[test]
fn array_test() {
    use crate::core::parser_gen::ParserGen;
    use crate::core::parser_methods::ParserMethods;

    let l_bracket_parser = ParserGen::char('[').with_skip_space();
    let r_bracket_parser = ParserGen::char(']').with_skip_space();
    let parser1 = ParserGen::dqoute_string_parser().with_skip_space();
    let separator = ParserGen::char(',').with_skip_space();
    let parser = array_parser(parser1, separator, l_bracket_parser, r_bracket_parser);


    match parser.parse("[]".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(Vec::<String>::new(), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse("[ \"abc\",\"def\" , \"ghi\"]".as_bytes(), 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(vec!["abc", "def", "ghi"], value);
            assert_eq!(22, location)
        }
        _ => assert!(false),
    }

}