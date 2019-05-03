use std::error::Error as StdError;
use std::fmt;

use super::lex::*; 
use super::parse::*; 

pub struct Interpreter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InterpreterErrorKind {
  DivisionByZero,
}

pub type InterpreterError = Annot<InterpreterErrorKind>;

impl Interpreter {
  pub fn new() -> Self {
    Interpreter
  }

    pub fn eval(&mut self, expr: &Ast) -> Result<i64, InterpreterError> {
      use self::AstKind::*;
      match expr.value {
        Num(n) => Ok(n as i64),
        UniOp { ref op, ref e } => {
          let e = self.eval(e)?;
          Ok(self.eval_uniop(op, e))
        }
        BinOp {
          ref op,
          ref l,
          ref r,
        } => {
          let l = self.eval(l)?;
          let r = self.eval(r)?;
          self.eval_binop(op, l, r)
          .map_err(|e| InterpreterError::new(e, expr.loc.clone()))
        }
      }
    }

    fn eval_uniop(&mut self, op: &UniOp, n: i64) -> i64 {
      use self::UniOpKind::*;
      match op.value {
        Plus => n,
        Minus => -n,
      }
    }
    fn eval_binop(&mut self, op: &BinOp, l: i64, r: i64) -> Result<i64, InterpreterErrorKind> {
      use self::BinOpKind::*;
      match op.value {
        Add => Ok(l + r),
        Sub => Ok(l - r),
        Mult => Ok(l * r),
        Div => {
          if r == 0 {
            Err(InterpreterErrorKind::DivisionByZero)
          } else {
            Ok(l / r)
          }
        }
      }
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::InterpreterErrorKind::*;
        match self.value {
            DivisionByZero => write!(f, "division by zero"),
        }
    }
}

impl StdError for InterpreterError {
    fn description(&self) -> &str {
        use self::InterpreterErrorKind::*;
        match self.value {
            DivisionByZero => "the right hand expression of the division evaluates to zero",
        }
    }
}

impl InterpreterError {
    pub fn show_diagnostic(&self, input: &str) {
        // エラー情報を簡単に表示し
        eprintln!("{}", self);
        // エラー位置を指示する
        print_annot(input, self.loc.clone());
    }
}

/// `input` に対して `loc` の位置を強調表示する
fn print_annot(input: &str, loc: Loc) {
    // 入力に対して
    eprintln!("{}", input);
    // 位置情報をわかりやすく示す
    eprintln!("{}{}", " ".repeat(loc.0), "^".repeat(loc.1 - loc.0));
}

/// 逆ポーランド記法へのコンパイラを表すデータ型
pub struct RpnCompiler;

impl RpnCompiler {
    pub fn new() -> Self {
        RpnCompiler
    }

    pub fn compile(&mut self, expr: &Ast) -> String {
        let mut buf = String::new();
        self.compile_inner(expr, &mut buf);
        buf
    }

    pub fn compile_inner(&mut self, expr: &Ast, buf: &mut String) {
        use self::AstKind::*;
        match expr.value {
            Num(n) => buf.push_str(&n.to_string()),
            UniOp { ref op, ref e } => {
                self.compile_uniop(op, buf);
                self.compile_inner(e, buf)
            }
            BinOp {
                ref op,
                ref l,
                ref r,
            } => {
                self.compile_inner(l, buf);
                buf.push_str(" ");
                self.compile_inner(r, buf);
                buf.push_str(" ");
                self.compile_binop(op, buf)
            }
        }
    }

    fn compile_uniop(&mut self, op: &UniOp, buf: &mut String) {
        use self::UniOpKind::*;
        match op.value {
            Plus => buf.push_str("+"),
            Minus => buf.push_str("-"),
        }
    }
    fn compile_binop(&mut self, op: &BinOp, buf: &mut String) {
        use self::BinOpKind::*;
        match op.value {
            Add => buf.push_str("+"),
            Sub => buf.push_str("-"),
            Mult => buf.push_str("*"),
            Div => buf.push_str("/"),
        }
    }
}