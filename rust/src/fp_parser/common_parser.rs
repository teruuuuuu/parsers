use crate::fp_parser::parser::*;

pub fn char_parser<'a>(c: char) -> Parser<'a, char> {
    Parser::new(move |input: &str, loc: Loc| {
        let chars = input.chars();
        let count = chars.count();
        let index = loc.0;
        if index >= count {
            ParseResult::ParseNg(format!("index invalid "), loc)
        } else {
            match input.chars().nth(index) {
                Some(d) if c == d => ParseResult::ParseOk::<char>(c, Loc(index+1, loc.1)),
                _ => ParseResult::ParseNg(format!("not match "), loc)
            }    
        }
    })
}