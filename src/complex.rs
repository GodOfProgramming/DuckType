use crate::ast::AstErr;
use crate::ast::{Evaluator, StatementType};
use crate::env::Env;
use crate::stmt::FunctionStmt;
use crate::types::Value;
use std::cell::RefCell;
use std::fmt::{self, Display};
use std::rc::Rc;

pub type CallResult = Result<Value, CallErr>;

pub struct CallErr {
  pub msg: String,
  pub line: usize,
}

impl From<AstErr> for CallErr {
  fn from(err: AstErr) -> Self {
    CallErr {
      msg: err.msg,
      line: err.line,
    }
  }
}

pub trait Callable: Display {
  fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>) -> CallResult;
}

pub struct NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  airity: usize,
  func: T,
}

impl<T> NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  pub fn new(airity: usize, func: T) -> NativeFunction<T> {
    NativeFunction { airity, func }
  }
}

impl<T> Display for NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<native function>")
  }
}

impl<T> Callable for NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  fn call(&self, _: &mut Evaluator, args: Vec<Value>) -> CallResult {
    if self.airity < args.len() {
      return Err(CallErr {
        msg: format!(
          "too many arguments, expected {}, got {}",
          self.airity,
          args.len()
        ),
        line: 0, // TODO
      });
    }

    if self.airity > args.len() {
      return Err(CallErr {
        msg: format!(
          "too few arguments, expected {}, got {}",
          self.airity,
          args.len(),
        ),
        line: 0, // TODO
      });
    }

    (self.func)(args)
  }
}

pub struct UserFunction {
  pub fun: Box<FunctionStmt>,
}

impl UserFunction {
  pub fn new(fun: Box<FunctionStmt>) -> UserFunction {
    UserFunction { fun }
  }
}

impl Display for UserFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fun {}>", self.fun.name)
  }
}

impl Callable for UserFunction {
  fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>) -> CallResult {
    let fun = &self.fun;
    if fun.params.len() < args.len() {
      return Err(CallErr {
        msg: format!(
          "too many arguments, expected {}, got {}",
          fun.params.len(),
          args.len()
        ),
        line: 0, // TODO
      });
    }

    if fun.params.len() > args.len() {
      return Err(CallErr {
        msg: format!(
          "too few arguments, expected {}, got {}",
          fun.params.len(),
          args.len(),
        ),
        line: 0, // TODO
      });
    }

    let env = Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(
      &evaluator.current_env,
    ))));

    for (param, arg) in fun.params.iter().zip(args.iter()) {
      if let Some(lexeme) = &param.lexeme {
        env.borrow_mut().define(lexeme.clone(), arg.clone())
      } else {
        return Err(CallErr {
          msg: String::from("no name in parameter"),
          line: param.line,
        });
      }
    }
    Ok(match evaluator.eval_block_ref(&fun.body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }
}
