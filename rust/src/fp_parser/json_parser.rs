
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::fp_parser::common_parser::*;
use crate::fp_parser::parser::*;
use crate::json_parser;

use super::parser::FunctorParser;
use super::parser_methods::ParserMethods;


#[derive(Clone)]
enum Parsers<'a> {
    Cons(Rc<RefCell<Parser<'a, JValue>>>, Rc<Parsers<'a>>),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JValue {
    JObject(HashMap<String, JValue>),
    JArray(Vec<JValue>),
    JString(String),
    JNumber(i64),
    JBool(bool),
    JNull
}


pub struct JsonParser<'a> {
    str_parser: Parser<'a, JValue>,
    num_parser: Parser<'a, JValue>,
    bool_parser: Parser<'a, JValue>,
    null_parser: Parser<'a, JValue>,
    array_parser: Rc<RefCell<Parser<'a, JValue>>>,
    object_parser: Rc<RefCell<Parser<'a, JValue>>>
}

impl <'a>JsonParser<'a> {
    fn new() -> JsonParser<'a> {
        let dummy_parer = Parser::new(move |input, loc| {
            println!("dummy parser");
            ParseResult::<JValue>::ParseNg("dummy".to_owned(), loc)
        });
    
        let str_parser = json_str_parser();
        let num_parser = json_num_parser();
        let bool_parser = json_bool_parser();
        let null_parser = json_null_parser();
        
        let array_parser_item = Rc::new(RefCell::new(dummy_parer.clone()));
        let object_parser_item = Rc::new(RefCell::new(dummy_parer.clone()));
        let str_parser_item = Rc::new(RefCell::new(str_parser.clone()));
        let num_parser_item = Rc::new(RefCell::new(num_parser.clone()));
        let bool_parser_item = Rc::new(RefCell::new(bool_parser.clone()));
        let null_parser_item = Rc::new(RefCell::new(null_parser.clone()));
    
        let parsers1 = Parsers::Cons(Rc::clone(&array_parser_item), Rc::new(Parsers::Nil));
        let parsers2 = Parsers::Cons(Rc::clone(&object_parser_item), Rc::new(parsers1)); 
        let parsers3 = Parsers::Cons(Rc::clone(&str_parser_item), Rc::new(parsers2)); 
        let parsers4 = Parsers::Cons(Rc::clone(&num_parser_item), Rc::new(parsers3)); 
        let parsers5 = Parsers::Cons(Rc::clone(&bool_parser_item), Rc::new(parsers4)); 
        let parsers6 = Parsers::Cons(Rc::clone(&null_parser_item), Rc::new(parsers5));
    
        let parser = json_value_parer(parsers6.clone());    
        *array_parser_item.borrow_mut() = json_array_parser(parser.clone());
        *object_parser_item.borrow_mut() = json_object_parser(parser.clone());

           
        JsonParser {
            str_parser,
            num_parser,
            bool_parser,
            null_parser,
            array_parser: array_parser_item,
            object_parser: object_parser_item,
        }
    }

    pub fn parse(&self, input: &'a str) -> ParseResult<JValue>{
        let res1 = self.object_parser.borrow().parse(input, Loc(0, input.len()));
        match res1 {
            ParseResult::ParseOk(v, l) => ParseResult::ParseOk(v, l),
            _ => self.array_parser.borrow().parse(input, Loc(0, input.len()))
        }
    }
}

#[test]
fn test_json_parser() {
    let parser = JsonParser::new();

    match parser.parse("{\"a\":{\"bcd\": [1,\"efg\",true,[2,3,4],{\"j\":false}]}}") {
        ParseResult::ParseOk(r, loc) => {
            println!("{:?}", r.clone());
            assert!(true)
        },
        _ => assert!(false)
    }

    match parser.parse("[[[1,true]],\"a\",21,true, null]") {
        ParseResult::ParseOk(r, loc) => {
            println!("{:?}", r.clone());
            assert!(true)
        },
        _ => assert!(false)
    }

}


fn json_str_parser<'a>() -> Parser<'a, JValue>{
    let left_double_quote_parser = char_parser('"');
    let str_parser = stop_word_parser("\"".to_owned());
    let right_double_quote_parser = char_parser('"');

    (left_double_quote_parser + str_parser + right_double_quote_parser).map(|a| JValue::JString(a.0.1))
}

#[test]
fn test_json_str_parser() {

    let parser = JsonParser::new();
    let input = "\"abc\": 10";
    
    match parser.str_parser.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            match r {
                JValue::JString(s) => {
                    assert_eq!("abc", s);
                    assert_eq!(Loc(5,9), loc);
                    assert!(true)
                },
                _ => assert!(false)
            }
            assert!(true)
        },
        _ => assert!(false)
    }
}

fn json_num_parser<'a>() -> Parser<'a, JValue> {
    Parser::new(|input, loc| {
        let parser = char_parser('1').or(char_parser('2')).or(char_parser('3')).or(char_parser('4')).or(char_parser('5')).
        or(char_parser('6')).or(char_parser('7')).or(char_parser('8')).or(char_parser('9')).or(char_parser('0')).
        repeat();
        match parser.parse(input, loc) {
            ParseResult::ParseOk(s, loc) if s.len() == 0 => ParseResult::ParseNg("()".to_owned(), loc),
            ParseResult::ParseOk(s, loc) => ParseResult::ParseOk(JValue::JNumber(s.iter().collect::<String>().parse::<i64>().unwrap()), loc),
            ParseResult::ParseNg(m, loc) => ParseResult::ParseNg(m, loc)
        }
    })
}

#[test]
fn test_json_num_parser() {
    let parser = json_num_parser();
    let input1 = "1234567890";
    
    match parser.parse(input1, Loc(0, input1.len())) {
        ParseResult::ParseOk(r, loc) => {
            match r {
                JValue::JNumber(v) => {
                    assert_eq!(1234567890, v);
                    assert_eq!(Loc(10, 10), loc);
                    assert!(true)
                },
                _ => assert!(false)
            }
            assert!(true)
        },
        _ => assert!(false)
    }

    let input2 = "g12345g67890";
    
    match parser.parse(input2, Loc(0, input2.len())) {
        ParseResult::ParseOk(_, _) => assert!(false),
        _ => assert!(true)
    }
}

fn json_bool_parser<'a>() -> Parser<'a, JValue> {
    let true_parer = (char_parser('T').or(char_parser('t')) + str_parser("rue".to_owned())).map(|_| JValue::JBool(true));
    let false_parer = (char_parser('F').or(char_parser('f')) + str_parser("alse".to_owned())).map(|_| JValue::JBool(false));

    true_parer.or(false_parer)
}

#[test]
fn test_json_bool_parser() {
    let parser = json_bool_parser();
    let input1 = "True";
    match parser.parse(input1, Loc(0, input1.len())) {
        ParseResult::ParseOk(r, loc) => {
            match r {
                JValue::JBool(v) => {
                    assert_eq!(true, v);
                    assert_eq!(Loc(4, 4), loc);
                    assert!(true)
                },
                _ => assert!(false)
            }
            assert!(true)
        },
        _ => assert!(false)
    }

    let input2 = "false";
    match parser.parse(input2, Loc(0, input2.len())) {
        ParseResult::ParseOk(r, loc) => {
            match r {
                JValue::JBool(v) => {
                    assert_eq!(false, v);
                    assert_eq!(Loc(5, 5), loc);
                    assert!(true)
                },
                _ => assert!(false)
            }
            assert!(true)
        },
        _ => assert!(false)
    }
}

fn json_null_parser<'a>() -> Parser<'a, JValue> {
    str_parser("null".to_owned()).or(str_parser("NULL".to_owned())).map(|_| JValue::JNull)
}

#[test]
fn test_json_null_parser() {
    let parser = json_null_parser();
    let input = "null";
    
    match parser.parse(input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            match r {
                JValue::JNull => {
                    assert_eq!(Loc(4, 4), loc);
                    assert!(true)
                },
                _ => assert!(false)
            }
            assert!(true)
        },
        _ => assert!(false)
    }
}

fn json_value_parer<'a>(jvalue_parers: Parsers<'a>) -> Parser<'a, JValue> {
    fn parse_value<'a>(parsers:&Parsers<'a>, input: &'a str, loc: Loc) -> ParseResult<JValue>{
        match parsers {
            Parsers::Cons(p1, p2) => {
                match (*p1.borrow()).parse(input, loc) {
                    ParseResult::ParseOk(r, loc) => ParseResult::ParseOk(r, loc),
                    _ => parse_value(p2.as_ref(), input, loc)
                }
            }
            Parsers::Nil => ParseResult::ParseNg("".to_owned(), loc)
        }
    }

    Parser::new(move |input, loc| {    
        parse_value(&jvalue_parers, input, loc)
    })
} 

fn json_array_parser<'a>(jvalue_parser: Parser<'a, JValue>) -> Parser<'a, JValue> {
    Parser::new(move |input, loc| {
        let lbracket_parer = char_parser('[');
        let skip_space_parser = char_parser(' ').repeat();

        let json_value_parser = (
            (skip_space_parser.clone() + jvalue_parser.clone()).map(|a| a.1) 
            + skip_space_parser.clone()).map(|a| a.0);

        let json_values_parser = (json_value_parser.clone() + (char_parser(',') + json_value_parser.clone())
            .map(|a| a.1).repeat())
            .map(|a| {
                let mut ret = vec![a.0];
                for jv in a.1 { ret.push(jv)};
                ret
            }).option().map(|a| match a {
                Some(k) => JValue::JArray(k),
                _ => JValue::JArray(Vec::new())
        });


        let rbracket_parer = char_parser(']');

        ((lbracket_parer.clone() + json_values_parser).map(|a| a.1) 
            + rbracket_parer.clone()).map(|a| a.0).parse(input, loc)
       
    })
}

#[test]
fn test_json_array_parser() {
    let parser = JsonParser::new();
    let input = "[[[1,true]],\"a\",21,true, null]";

    let result = parser.array_parser.borrow().parse(input, Loc(0, input.len()));
    

    let ans = JValue::JArray(vec![
        JValue::JArray(vec![
            JValue::JArray(vec![
                JValue::JNumber(1), JValue::JBool(true)
            ])
        ])
        , JValue::JString("a".to_owned())
        , JValue::JNumber(21)
        , JValue::JBool(true)
        , JValue::JNull
    ]);
    match result {
        ParseResult::ParseOk(r, loc) => {
            println!("{:?}", ans);
            println!("{:?}", r.clone());
            match r {
                JValue::JArray(s) => {
                    println!("{:?}", s);
                    assert!(true)
                },
                _ => assert!(false)
            }
            
            assert!(true)
        },
        _ => assert!(false)
    }
}

fn json_object_parser<'a>(jvalue_parser: Parser<'a, JValue>) -> Parser<'a, JValue> {
    Parser::new(move |input, loc| {
        let lbracket_parer = char_parser('{');
        let skip_space_parser = char_parser(' ').repeat();

        fn str_parser_gen<'a>() -> Parser<'a, String>{
            let left_double_quote_parser = char_parser(' ').repeat() + char_parser('"');
            let str_parser = stop_word_parser("\"".to_owned());
            let right_double_quote_parser = char_parser('"') + char_parser(' ').repeat();
            (left_double_quote_parser + str_parser + right_double_quote_parser).map(|a| a.0.1)
        }
        let str_parser = str_parser_gen();
        let json_value_parser = (
            (skip_space_parser.clone() + jvalue_parser.clone()).map(|a| a.1) 
            + skip_space_parser.clone()).map(|a| a.0);

        let json_value_set_parser: Parser<'a, (String, JValue)> = 
            (str_parser.clone() + char_parser(':') + json_value_parser.clone()).map(|a| (a.0.0, a.1));

        let json_values_parser = (json_value_set_parser.clone() + (char_parser(',') + json_value_set_parser.clone())
            .map(|a| a.1).repeat())
            .map(|a| {
                let mut ret = vec![a.0];
                for jv in a.1 { ret.push(jv)};
                ret
            }).option().map(|a| match a {
                Some(k) => {
                    JValue::JObject(k.into_iter().collect::<HashMap<_, _>>())
                },
                _ => JValue::JObject(HashMap::new())
        });

        let rbracket_parer = char_parser('}');

        ((lbracket_parer.clone() + json_values_parser).map(|a| a.1) 
            + rbracket_parer.clone()).map(|a| a.0).parse(input, loc)
       
    })
}

#[test]
fn test_json_object_parser() {
    let parser = JsonParser::new();
    let input = "{\"a\":{\"bcd\": [1,\"efg\",true,[2,3,4],{\"j\":false}]}}";

    let result = parser.object_parser.borrow().parse(input, Loc(0, input.len()));
    

    let ans = JValue::JArray(vec![
        JValue::JArray(vec![
            JValue::JArray(vec![
                JValue::JNumber(1), JValue::JBool(true)
            ])
        ])
        , JValue::JString("a".to_owned())
        , JValue::JNumber(21)
        , JValue::JBool(true)
        , JValue::JNull
    ]);
    match result {
        ParseResult::ParseOk(r, loc) => {
            println!("{:?}", r.clone());
            assert!(true)
        },
        _ => assert!(false)
    }
}