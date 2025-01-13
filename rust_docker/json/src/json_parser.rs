use std::cell::UnsafeCell;
use std::rc::Rc;

use rcombinator::prelude::ParseResult;
use rcombinator::prelude::ParserGen;
use rcombinator::prelude::*;
use crate::jvalue::JValue;

fn jarray_parser_gen<'a>(parse_func_vec: &mut Vec<Rc<UnsafeCell<Parser<'a, JValue>>>> ) -> Parser<'a, JValue> {

    Parser::new(Rc::new(move |_, location| {
        ParseResult::Success { value: JValue::JNull, location }
    }))
}

fn jobject_parser_gen<'a>(parse_func_vec: &mut Vec<Rc<UnsafeCell<Parser<'a, JValue>>>> ) -> Parser<'a, JValue> {

    Parser::new(Rc::new(move |_, location| {
        ParseResult::Success { value: JValue::JNull, location }
    }))
}

fn json_parser<'a>() -> Parser<'a, JValue> 
{
    unsafe {
        let mut parse_vec: Vec<Rc<UnsafeCell<Parser<'_, JValue>>>> = Vec::new();
        let jstr_parser = Rc::new(UnsafeCell::new(ParserGen::dquote_string().map(|str| JValue::JString(str))));
        parse_vec.push(jstr_parser.clone());
        let jnumber_parser = Rc::new(UnsafeCell::new(ParserGen::number().map(|v| JValue::JNumber(v))));
        parse_vec.push(jnumber_parser.clone());
        let jbool_parser: Rc<UnsafeCell<Parser<'_, JValue>>> = Rc::new(UnsafeCell::new(ParserGen::str("true".to_owned()).pure(JValue::JBool(true)).or(ParserGen::str("true".to_owned()).pure(JValue::JBool(true)))));
        parse_vec.push(jbool_parser.clone());


        let jarray_parser: Rc<UnsafeCell<Parser<'_, JValue>>> = Rc::new(UnsafeCell::new(
            Parser::new(Rc::new(move |_, location| {
                ParseResult::Success { value: JValue::JNull, location }
            }))
        ));
        parse_vec.push(jarray_parser.clone());
        
        let jobject_parser: Rc<UnsafeCell<Parser<'_, JValue>>> = Rc::new(UnsafeCell::new(
            Parser::new(Rc::new(move |_, location| {
                ParseResult::Success { value: JValue::JNull, location }
            }))
        ));
        parse_vec.push(jobject_parser.clone());
        

        let parse_vec_clone = parse_vec.clone();
        let jvalue_parser_func = move |input, location| {
            parse_vec_clone.iter().find_map(|parse_func| match (&*parse_func.get()).parse_inner(input, location) {
                ParseResult::Success { value, location } => Some(ParseResult::Success { value, location }),
                _ => None,
            })
            .unwrap_or_else(|| ParseResult::Failure {
                parse_error: ParseError {
                    label: "json".to_string(),
                    message: "not json".to_string(),
                    location,
                    children: vec![],
                },
                location,
            })
        };

        let jvalue_parser = Parser::new(Rc::new(jvalue_parser_func));

        

        (*jarray_parser.get()) = jvalue_parser.clone().with_skip_space().array(
            ParserGen::char(',').with_skip_space(), 
            ParserGen::char('[').with_skip_space(), 
            ParserGen::char(']').with_skip_space()
        ).map(move |v| JValue::JArray(v));

        (*jobject_parser.get()) = ParserGen::dquote_string().with_skip_space()
            .and_left(ParserGen::char(':').with_skip_space())
            .and(jvalue_parser.clone().with_skip_space()).array(
                ParserGen::char(',').with_skip_space(), 
                ParserGen::char('{').with_skip_space(), 
                ParserGen::char('}').with_skip_space()
        ).map(move |v| JValue::JObject(v.into_iter().collect()));


        let parser_func = move |input, location| {
            let parse_func_vec = vec![
                (*jarray_parser.get()).parse_func.clone(), (*jobject_parser.get()).parse_func.clone()
            ];

            parse_func_vec.into_iter().find_map(|parse_func| match parse_func(input, location) {
                ParseResult::Success { value, location } => Some(ParseResult::Success { value, location }),
                _ => None,
            })
            .unwrap_or_else(|| ParseResult::Failure {
                parse_error: ParseError {
                    label: "json".to_string(),
                    message: "not json".to_string(),
                    location,
                    children: vec![],
                },
                location,
            })
        };
        Parser::new(Rc::new(parser_func))
    }
}


#[test]
fn test_json() {
    let parser = json_parser();
    let parse_result = parser.parse(&"[1,2,3,[\"abc\", \"def\", 4, 5, 6]]", 0);
    println!("{:?}", parse_result);

    let mut parse_result = parser.parse(&"{\"a\":[1,2,3,[\"abc\", \"def\", 4, 5, 6, {}]]}", 0);
    println!("{:?}", parse_result);


    use std::time::{Duration, Instant};
    use std::thread::sleep;

    let start = Instant::now();
    for i in 0..1000000 {
        parse_result =  parser.parse(&"  [ 123, true, [456,  { \"string\": \"aaaaa\", \"numberInt\": 123, \"numberDouble\": -123.456, \"bool\": true, \"null\": null,\"array\":[ [1]] } ] ]", 0);
    }
    let end = start.elapsed();
    println!("time: {}.{:03}", end.as_secs(), end.subsec_nanos() / 1_000_000);
}