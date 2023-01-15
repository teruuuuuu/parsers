
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::cell::RefMut;
use std::char;
use std::collections::HashMap;
use std::rc::Rc;
use crate::fp_parser::common_parser::*;
use crate::fp_parser::parser::*;

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
    // array_parser: Parser<'a, JValue>,
    // jvalue_parsers: &'a Vec<Rc<Parser<'a, JValue>>>,
}

impl <'a>JsonParser<'a> {
    fn new() -> JsonParser<'a> {
        let dummy_parer = Parser::new(move |input, loc| ParseResult::<JValue>::ParseNg("dummy".to_owned(), loc));

        let array_parser = Rc::new(RefCell::new(dummy_parer.clone()));
        let object_parser = Rc::new(RefCell::new(dummy_parer.clone()));
        let str_parser = Rc::new(RefCell::new(json_str_parser()));
        let num_parser = Rc::new(RefCell::new(json_num_parser()));
        let bool_parser = Rc::new(RefCell::new(json_bool_parser()));
        let null_parser = Rc::new(RefCell::new(json_null_parser()));


        // let a = Rc::new(Parsers::Cons(Rc::clone(&value), Rc::new(Parsers::Nil)));
        // let b = Parsers::Cons(Rc::new(RefCell::new(json_str_parser())), Rc::clone(&a));
        // let c = Parsers::Cons(Rc::new(RefCell::new(json_str_parser())), Rc::clone(&a));

        let parsers1 = Parsers::Cons(Rc::clone(&array_parser), Rc::new(Parsers::Nil));
        let parsers2 = Parsers::Cons(Rc::clone(&object_parser), Rc::new(parsers1)); 
        let parsers3 = Parsers::Cons(Rc::clone(&str_parser), Rc::new(parsers2)); 
        let parsers4 = Parsers::Cons(Rc::clone(&num_parser), Rc::new(parsers3)); 
        let parsers5 = Parsers::Cons(Rc::clone(&bool_parser), Rc::new(parsers4)); 
        let parsers6 = Parsers::Cons(Rc::clone(&null_parser), Rc::new(parsers5)); 
    

        // *array_parser = RefCell::new(json_bool_parser());




        let mut jvalue_parsers = RefCell::new(Vec::new());


        let str_parser = json_str_parser();
        let num_parser = json_num_parser();
        let bool_parser = json_bool_parser();
        let null_parser = json_null_parser();

        
        jvalue_parsers.borrow_mut().push(Rc::from(str_parser.clone()));
        jvalue_parsers.borrow_mut().push(Rc::from(num_parser.clone()));
        jvalue_parsers.borrow_mut().push(Rc::from(bool_parser.clone()));
        // let array_parser = json_array_parser(jvalue_parsers.borrow_mut());
        // jvalue_parsers.borrow_mut().push(Rc::from(array_parser.clone()));        


        JsonParser {
            str_parser,
            num_parser,
            bool_parser,
            null_parser,
            // array_parser,
            // jvalue_parsers: jvalue_parsers
        }
    }

    fn parse_jvalue(&self, input: &'a str, loc: Loc) -> ParseResult<JValue> {
        let mut p_reult = self.str_parser.parse(input, loc);
        if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.num_parser.parse(input, loc);}
        if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.bool_parser.parse(input, loc);}
        if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.null_parser.parse(input, loc);}
        if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.parse_jarray(input, loc);}

        p_reult
    }

    fn parse_jarray(&self, input: &'a str, loc: Loc) -> ParseResult<JValue> {
        let skip_space_parser = char_parser(' ').repeat();
        let lbracket_parser = (char_parser('[') + skip_space_parser.clone());
        let rbracket_parser = (skip_space_parser.clone() + char_parser(']'));

        let vec = Vec::new();

        let parse_result = match lbracket_parser.parse(input, loc) {
            ParseResult::ParseOk(_, loc1) => {
                
                loop {
                    let mut p_reult = self.str_parser.parse(input, loc1);
                    if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.num_parser.parse(input, loc1);}
                    if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.num_parser.parse(input, loc1);}
                    if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.num_parser.parse(input, loc1);}
                    if let ParseResult::ParseNg(_, _) = p_reult {p_reult = self.num_parser.parse(input, loc1);}

                }

                ParseResult::ParseNg::<JValue>("".to_owned(), loc)
            },
            _ => ParseResult::ParseNg("".to_owned(), loc)
        };

        match parse_result {
            ParseResult::ParseOk(_, loc) => ParseResult::ParseOk(JValue::JArray(vec), loc),
            _ => ParseResult::ParseNg("".to_owned(), loc)
        }
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
    char_parser('1').or(char_parser('2')).or(char_parser('3')).or(char_parser('4')).or(char_parser('5')).
        or(char_parser('6')).or(char_parser('7')).or(char_parser('8')).or(char_parser('9')).or(char_parser('0')).
        repeat().map(|r| JValue::JNumber(r.iter().collect::<String>().parse::<i64>().unwrap()))
}

#[test]
fn test_json_num_parser() {
    let parser = json_num_parser();
    let input = "1234567890";
    
    match parser.parse(input, Loc(0, input.len())) {
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

// fn json_value_parer<'a>(jvalue_parers: RefMut<Vec<Rc<Parser<'a, JValue>>>>) -> Parser<'a, JValue> {
//     Parser::new(move |input, loc| {
//         println!("parse start");
//         let mut res = ParseResult::ParseNg("".to_owned(), loc);
//         for parser in jvalue_parers.borrow().iter() {
//             println!("parse!!!");
//             res = parser.parse(input, loc);
//             match res {
//                 ParseResult::ParseOk(_, _) => break,
//                 _ => {}
//             }
//         };
//         println!("parse end");
//         res
//     })
// } 

// fn json_array_parser<'a>(jvalue_parsers: RefMut<Vec<Rc<Parser<'a, JValue>>>>) -> Parser<'a, JValue> {

//     Parser::new(move |input, loc| {
//         let lbracket_parer = char_parser('[');
//         let skip_space_parser = char_parser(' ').repeat();
//         let json_value_parser = (
//             (skip_space_parser.clone() + json_value_parer(jvalue_parsers.clone())).map(|a| a.1) 
//             + skip_space_parser.clone()).map(|a| a.0);

//         let json_values_parser = (json_value_parser.clone() + (char_parser(',') + json_value_parser.clone())
//             .map(|a| a.1).repeat())
//             .map(|a| {
//                 let mut ret = vec![a.0];
//                 for jv in a.1 { ret.push(jv)};
//                 ret
//             }).option().map(|a| match a {
//                 Some(k) => JValue::JArray(k),
//                 _ => JValue::JArray(Vec::new())
//             });

//         let rbracket_parer = char_parser(']');

//         ((lbracket_parer.clone() + json_values_parser.clone()).map(|a| a.1) 
//             + rbracket_parer.clone()).map(|a| a.0).parse(input, loc)
       
//     })
// }

#[test]
fn test_json_array_parser() {


    // let parser = JsonParser::new();
    // // let input = "[1, \"abc\" ,[]]";
    // let input = "[[]]";
    
    // match parser.array_parser.parse(input, Loc(0, input.len())) {
    //     ParseResult::ParseOk(r, loc) => {
    //         match r {
    //             JValue::JArray(s) => {
    //                 assert!(true)
    //             },
    //             _ => assert!(false)
    //         }
    //         assert!(true)
    //     },
    //     _ => assert!(false)
    // }

    fn parse_value<'a>(parsers:&Parsers<'a>, input: &'a str, loc: Loc) -> ParseResult<JValue>{
        match parsers {
            Parsers::Cons(p1, p2) => {
                println!("parse!");
                match (*p1.borrow()).parse(input, loc) {
                    ParseResult::ParseOk(r, loc) => ParseResult::ParseOk(r, loc),
                    _ => parse_value(p2.as_ref(), input, loc)
                }
            }
            Parsers::Nil => ParseResult::ParseNg("".to_owned(), loc)
        }
    }

    let dummy_parer = Parser::new(move |input, loc| ParseResult::<JValue>::ParseNg("dummy".to_owned(), loc));

    let array_parser = Rc::new(RefCell::new(dummy_parer.clone()));
    let object_parser = Rc::new(RefCell::new(dummy_parer.clone()));
    let str_parser = Rc::new(RefCell::new(json_str_parser()));
    let num_parser = Rc::new(RefCell::new(dummy_parer.clone()));
    let bool_parser = Rc::new(RefCell::new(json_bool_parser()));
    let null_parser = Rc::new(RefCell::new(json_null_parser()));


    // let a = Rc::new(Parsers::Cons(Rc::clone(&value), Rc::new(Parsers::Nil)));
    // let b = Parsers::Cons(Rc::new(RefCell::new(json_str_parser())), Rc::clone(&a));
    // let c = Parsers::Cons(Rc::new(RefCell::new(json_str_parser())), Rc::clone(&a));

    let parsers1 = Parsers::Cons(Rc::clone(&array_parser), Rc::new(Parsers::Nil));
    let parsers2 = Parsers::Cons(Rc::clone(&object_parser), Rc::new(parsers1)); 
    let parsers3 = Parsers::Cons(Rc::clone(&str_parser), Rc::new(parsers2)); 
    let parsers4 = Parsers::Cons(Rc::clone(&num_parser), Rc::new(parsers3)); 
    let parsers5 = Parsers::Cons(Rc::clone(&bool_parser), Rc::new(parsers4)); 
    let parsers6 = Parsers::Cons(Rc::clone(&null_parser), Rc::new(parsers5)); 

    let input = "\"2\"";

    match parse_value(&parsers6, input, Loc(0, input.len())) {
        ParseResult::ParseOk(r, loc) => {
            assert!(true)
        },
        _ => assert!(false)
    }
    
}