// use std::rc::Rc;

// use crate::core::parser_gen::ParserGen;

// use crate::core::parser::{Parser, ParserTrait, ParserFunctor, ParserMonad};
// use crate::core::parse_result::ParseResult;
// use crate::core::parser_methods::ParserMethods;

// use crate::core::util::or_parser::or_parser;


// use crate::json::jvalue::JValue;

// fn json_parser<'a>() -> Parser<'a, JValue, impl Fn(&'a [u8], usize) -> ParseResult<JValue> + Clone + 'a> 
// {

//     // let mut parse_funcs     = Vec::new();

//     // let jstr_parser = ParserGen::dqoute_string_parser().map(|str| JValue::JString(str));
//     // let jnumber_parser = ParserGen::number().map(|v| JValue::JNumber(v));
//     // let jbool_parser = or_parser(
//     //     ParserGen::string("true".to_owned()).pure(JValue::JBool(true))
//     //     , ParserGen::string("true".to_owned()).pure(JValue::JBool(true))
//     // );

//     // // let a = &jstr_parser.parse.clone();
//     // // let b : Rc<&= Rc::new(&jstr_parser.parse.clone());


//     // let str_parser_func = ParserGen::dqoute_string_parser().parse;
//     // let number_parse_func = ParserGen::number().parse;

//     // let fjstr_parser_func = move |input, location| {
//     //     match str_parser_func(input, location) {
//     //         ParseResult::Success { value, location } => {
//     //             ParseResult::Success { value: JValue::JString(value), location }
//     //         },
//     //         ParseResult::Failure {parse_error, location} => {
//     //             ParseResult::Failure {parse_error, location}
//     //         }
//     //     }
//     // };

//     // let jnumber_parse_func = move |input, location| {
//     //     match str_parser_func(input, location) {
//     //         ParseResult::Success { value, location } => {
//     //             ParseResult::Success { value: JValue::JString(value), location }
//     //         },
//     //         ParseResult::Failure {parse_error, location} => {
//     //             ParseResult::Failure {parse_error, location}
//     //         }
//     //     }
//     // };

//     // vec![Rc::new(&fjstr_parser_func), Rc::new(&jnumber_parse_func)];

//     // parse_funcs.push(Rc::new(&jstr_parser.parse.clone()));
//     // parse_funcs.push(Rc::new(&jnumber_parser.parse.clone()));
//     // parse_funcs.push(Rc::new(&jbool_parser.parse.clone()));



//     let f = move |input, location| {
//         ParseResult::Success { value: JValue::JNull, location }  
//     };
//     Parser::new(Rc::new(f))

// }