use crate::fp_parser::parser::*;

use super::parser_methods::ParserMethods;

pub fn any_parser<'a>() -> Parser<'a, char> {
    Parser::new(move |input, loc| match input.chars().nth(loc.0) {
        Some(c) => ParseResult::ParseOk(c, Loc(loc.0+1, loc.1)),
        _ => ParseResult::ParseNg("()".to_owned(), loc)
    }
)
}

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

#[test]
fn test_char_parser() {
    let p = char_parser('a');
    let input = "abcd";
    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => assert!(true),
        ParseResult::ParseNg(m, loc) => assert!(false)
    }
}

pub fn str_parser<'a>(s: String) -> Parser<'a, String> {
    Parser::new(move |input, loc| {
        let cur = &input[loc.0..];
        if(cur.starts_with(&s)) {
            ParseResult::ParseOk(s.to_owned(), Loc(loc.0 + s.len(), loc.1))
        } else {
            ParseResult::ParseNg("not match ".to_owned(), loc)
        }
    }) 
}

#[test]
fn test_str_parser() {
    let p = (str_parser("ab".to_owned()) + str_parser("cd".to_owned())).map(|r| format!("{}{}",r.0, r.1));
    
    
    let input = "abcd";
    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            println!("result[{}]", r);
            assert!(true)
        },
        ParseResult::ParseNg(m, loc) => assert!(false)
    }
}

pub fn stop_word_parser<'a>(s: String) -> Parser<'a, String> {
    (str_parser(s).not() + any_parser()).map(|a| a.1).repeat().map(|r| r.iter().collect::<String>())
}

#[test]
fn test_stop_word_parser() {
    let p = stop_word_parser("\"".to_owned());
    let input = "aaaaaaaaa\"bc";

    match p.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            assert_eq!("aaaaaaaaa", r);
            assert_eq!(Loc(9, 12), loc);
            assert!(true)
        }
        _ => assert!(false)
    }
}
