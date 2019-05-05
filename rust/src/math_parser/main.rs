use std::error::Error as StdError;
use std::fmt;
use std::io;

use super::lex::*;
use super::parser::*;
use super::interpreter::*;

pub fn main() {
  use std::io::{stdin, BufRead, BufReader};
  let mut interp = Interpreter::new();
  let mut compiler = RpnCompiler::new();

  let stdin = stdin();
  // 共有資源アクセスのために1バイト読み込むごとにロック、アンロックを繰り返すと時間がかかるから、
  // ここで明示的にlockしておいて共有資源じゃ無いようにして奥
  let stdin = stdin.lock();
  let stdin = BufReader::new(stdin);
  let mut lines = stdin.lines();

  loop {
    prompt("> ").unwrap();
    if let Some(Ok(line)) = lines.next() {
      let tokens = lex(&line).unwrap();
      let ast = parse(tokens).unwrap();
      println!("{:?}", ast);
      let n = match interp.eval(&ast) {
        Ok(n) => n,
        Err(e) => {
          e.show_diagnostic(&line);
          show_trace(e);
          continue;
        }
      };
      let rpn = compiler.compile(&ast);
      println!("{}", rpn);
      println!("{}", n);
    } else {
      break;
    }
  }

  loop {
    prompt("> ").unwrap();
    // ユーザの入力を取得する
    if let Some(Ok(line)) = lines.next() {
      // `from_str` を実装したので`parse`が呼べる
      let ast = match line.parse::<Ast>() {
        Ok(ast) => ast,
        Err(e) => {
          show_trace(e);
          continue;
          }
        };
      println!("{:?}", ast);
      } else {
        break;
      }
    }
}


fn prompt(s: &str) -> io::Result<()> {
  use std::io::{stdout, Write};
  let stdout = stdout();
  let mut stdout = stdout.lock();
  stdout.write(s.as_bytes())?;
  stdout.flush()
}

fn show_trace<E: StdError>(e: E) {
    // エラーがあった場合そのエラーとcauseを全部出力する
    eprintln!("{}", e);
    let mut source = e.source();
    // cause を全て辿って表示する
    while let Some(e) = source {
        eprintln!("caused by {}", e);
        source = e.source()
    }
    // エラー表示のあとは次の入力を受け付ける
}