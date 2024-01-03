use std::cell::RefCell;
use std::rc::Rc;

use crate::fp_parser::parser::Loc;

type Parse<'a> = dyn Fn(&'a str, usize) -> (bool, usize) + 'a;

pub struct Parser<'a> {
    pub parse: Rc<Parse<'a>>
}

impl <'a>Parser<'a> {
    pub fn new<F>(parse: F) -> Parser<'a>
    where
      F: Fn(&'a str, usize) -> (bool, usize)+ 'a {Parser { parse: Rc::new(parse)}}
    
    fn parse(&self,input: &'a str, loc: usize ) -> (bool, usize) {
        (self.parse)(input, loc)
    }
}


fn char_parser<'a>(c: char) -> Parser<'a> {
    Parser::new(move |input, loc| {
        match input.chars().nth(loc) {
            Some(d) if c == d => (true, loc + 1),
            _ => (false, loc)
        }  
    })
}

#[test]
fn test_char_parser() {
    let a_parser = char_parser('a');
    
    assert_eq!((true, 1), a_parser.parse("abc", 0));
    assert_eq!((false, 5), a_parser.parse("abc", 5));
}

fn and_parser<'a>(parser1: Parser<'a>, parser2: Parser<'a>) -> Parser<'a> {
    Parser::new(move |input, loc| {
        match parser1.parse(input, loc) {
            (true, loc) => parser2.parse(input, loc),
            (false, loc) => (false, loc)
        }
    })
}

#[test]
fn test_and_parser() {
    let a_parser = char_parser('a');
    let b_parser = char_parser('b');
    let ab_parser = and_parser(a_parser, b_parser);

    assert_eq!((true, 2), ab_parser.parse("abc", 0));
    assert_eq!((false, 5), ab_parser.parse("abc", 5));
}

fn array_parser<'a>(parsers: Vec<Parser<'a>>) -> Rc<RefCell<Parser<'a>>> {
    fn dummy_parer<'b>() -> Parser<'b>{
        Parser::new(|_,loc| (false, loc))
    }
    // 一旦ダミーを入れて初期化
    let array_parser_item = Rc::new(RefCell::new(dummy_parer()));


    let mut parser_items = parsers.into_iter()
        .map(|a| Rc::new(RefCell::new(a)))
        .collect::<Vec<Rc<RefCell<Parser<'a>>>>>();
    parser_items.push(Rc::clone(&array_parser_item));

    fn parser_gen<'a>(paser_items: Vec<Rc<RefCell<Parser<'a>>>>) -> Parser<'a> {
        let left_parser = char_parser('[');
        let right_parser = char_parser(']');
        let comma_parser = char_parser(',');
        Parser::new(move|input, loc| {
                match left_parser.parse(input, loc) {
                    (true, loc) => {
                        let mut cur_loc = loc;
                        let mut first = true;
                        let mut is_ng = false;
                        loop {
                            if(!first) {
                                match comma_parser.parse(input, cur_loc) {
                                    (true, loc) => cur_loc = loc,
                                    (false, loc) => break
                                }
                            }
                            let mut find = false;
                            for p in paser_items.iter() {
                                match p.borrow().parse(input, cur_loc) {
                                    (true, loc) => {
                                        find = true;
                                        cur_loc = loc;
                                        break;
                                    },
                                    _ => {}
                                }
                            }
                            first = false;
                            is_ng = !(!first && find);
                            if(!find) {
                                break;
                            }
                        }
                        if (!is_ng) {
                            right_parser.parse(input, cur_loc)
                        } else {
                            (false, cur_loc)
                        }
                    },
                    (false, loc) => (false, loc)
                }
        })
    }
    let array_parsre = parser_gen(parser_items);

    *array_parser_item.borrow_mut() = array_parsre;
    array_parser_item
}

#[test]
fn test_array_parser() {
    let a_parser = char_parser('a');
    let b_parser = char_parser('b');
    let cd_parser = and_parser(char_parser('c'), char_parser('d'));
    let parser = array_parser(vec![a_parser, b_parser, cd_parser]);

    assert_eq!((true, 16), parser.borrow().parse("[a,b,[a,[cd]],b]", 0));
    assert_eq!((false, 7), parser.borrow().parse("[a,b,a,ef,b]", 0));
    assert_eq!((false, 7), parser.borrow().parse("[a,b,a,[,b]", 0));
}

pub trait Messenger {
    fn send(&self, msg: &str);
}

pub struct LimitTracker<'a, T: 'a + Messenger> {
    messenger: &'a T,
    value: usize,
    max: usize,
}

impl<'a, T> LimitTracker<'a, T>
    where T: Messenger {
    pub fn new(messenger: &T, max: usize) -> LimitTracker<T> {
        LimitTracker {
            messenger,
            value: 0,
            max,
        }
    }

    pub fn set_value(&mut self, value: usize) {
        self.value = value;

        let percentage_of_max = self.value as f64 / self.max as f64;

        if percentage_of_max >= 0.75 && percentage_of_max < 0.9 {
            // 警告: 割り当ての75％以上を使用してしまいました
            self.messenger.send("Warning: You've used up over 75% of your quota!");
        } else if percentage_of_max >= 0.9 && percentage_of_max < 1.0 {
            // 切迫した警告: 割り当ての90%以上を使用してしまいました
            self.messenger.send("Urgent warning: You've used up over 90% of your quota!");
        } else if percentage_of_max >= 1.0 {
            // エラー: 割り当てを超えています
            self.messenger.send("Error: You are over your quota!");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::sync::{Arc, Mutex};
    use std::time::Duration;

    struct MockMessenger {
        sent_messages: RefCell<Vec<String>>,
    }

    impl MockMessenger {
        fn new() -> MockMessenger {
            MockMessenger { sent_messages: RefCell::new(vec![]) }
        }
    }

    impl Messenger for MockMessenger {
        fn send(&self, message: &str) {
            self.sent_messages.borrow_mut().push(String::from(message));
        }
    }

    #[test]
    fn it_sends_an_over_75_percent_warning_message() {
        let mock_messenger = MockMessenger::new();
        let mut limit_tracker = LimitTracker::new(&mock_messenger, 100);

        limit_tracker.set_value(80);

        assert_eq!(mock_messenger.sent_messages.borrow_mut().len(), 1);
    }

    #[test]
    fn test_arc_mutex() {
        

        let arc_mock_messenger = Arc::new(Mutex::new(MockMessenger::new()));

        let ref1 = Arc::clone(&arc_mock_messenger);
        let ref2 = Arc::clone(&arc_mock_messenger);
        let ref3 = Arc::clone(&arc_mock_messenger);

        let th1 = thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));
            ref1.lock().unwrap().send("abc");
        });
        let th2 = thread::spawn(move || {
            thread::sleep(Duration::from_secs(1));
            ref2.lock().unwrap().send("def");
        });

        th1.join().unwrap();
        th2.join().unwrap();

        thread::spawn(move || {
            for m in ref3.lock().unwrap().sent_messages.borrow_mut().iter() {
                println!("{:?}", m);
            }
        });
    }

    struct MockMessenger2 {
        sent_messages: Arc<Mutex<Vec<String>>>,
    }

    impl MockMessenger2 {
        fn new() -> MockMessenger2 {
            MockMessenger2 { sent_messages: Arc::new(Mutex::new(vec![])) }
        }
    }

    // impl Messenger for MockMessenger2 {
    //     fn send(&self, message: &str) {
    //         self.sent_messages.borrow_mut().push(String::from(message));
    //     }
    // }


}

