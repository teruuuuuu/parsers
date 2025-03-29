use crate::core::parser::Parser;
use crate::core::parse_result::ParseResult;

use crate::util::item::bracket_parser::bracket_parser;
use crate::util::item::separate0_parser::separate0_parser;

pub fn array_parser<'a, A, B, C>(parser: Parser<'a, A>, separate_parser: Parser<'a, B>, l_bracket_parser: Parser<'a, C>, r_bracket_parser: Parser<'a, C>) -> Parser<'a, Vec<A>>
where
    A: Clone + 'a,
    B: Clone + 'a,
    C: Clone + 'a,
{
    bracket_parser(separate0_parser(parser, separate_parser), l_bracket_parser, r_bracket_parser)
}


#[test]
fn array_test() {
    use crate::core::parser::*;
    use crate::util::parser_gen::ParserGen;
    use crate::util::parser_methods::ParserMethods;

    let l_bracket_parser = ParserGen::char('[').with_skip_space();
    let r_bracket_parser = ParserGen::char(']').with_skip_space();
    let parser1 = ParserGen::dquote_string().with_skip_space();
    let separator = ParserGen::char(',').with_skip_space();
    let parser = array_parser(parser1, separator, l_bracket_parser, r_bracket_parser);


    match parser.parse(&"[]", 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(Vec::<String>::new(), value);
            assert_eq!(2, location)
        }
        _ => assert!(false),
    }

    match parser.parse(&"[ \"abc\",\"def\" , \"ghi\"]", 0) {
        ParseResult::Success { value, location } => {
            assert!(true);
            assert_eq!(vec!["abc", "def", "ghi"], value);
            assert_eq!(22, location)
        }
        _ => assert!(false),
    }

}