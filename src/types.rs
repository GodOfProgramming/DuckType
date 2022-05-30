use crate::{
  code::{Context, OpCode, OpCodeReflection},
  New,
};
use ptr::SmartPtr;
use std::{
  cmp::{Ordering, PartialEq, PartialOrd},
  collections::BTreeMap,
  fmt::{self, Debug, Display},
  ops::{Add, Div, Index, IndexMut, Mul, Neg, Not, Rem, Sub},
};

pub trait Interpreter {
  fn interpret(&self, ctx: &mut Context) -> Result<Value, Error>;
}

#[derive(Default, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl Error {
  pub fn from_ref(msg: String, opcode: &OpCode, opcode_ref: OpCodeReflection) -> Self {
    let mut e = Self {
      msg,
      file: opcode_ref.file.clone(),
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    e.format_with_src_line(opcode_ref.source_line);
    e.msg = format!("{}\nOffending OpCode: {:?}", e.msg, opcode);
    e
  }

  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!("{}\n{}\n{}^", self.msg, src, " ".repeat(self.column - 1));
  }
}

impl Debug for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

pub type NativeFn = dyn FnMut(&[Value]) -> ValueOpResult;
pub type NativeFnPtr = Box<dyn FnMut(&[Value]) -> ValueOpResult>;
pub type NativeFnSmartPtr = SmartPtr<NativeFnPtr>;

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Num(f64),
  Str(String),
  List(Values),
  Function(Function),
  NativeFunction(NativeFnSmartPtr),

  U128(u128), // internal use only
}

impl Value {
  pub fn truthy(&self) -> bool {
    if *self == Value::Nil {
      return false;
    }

    if let Value::Bool(b) = self {
      return *b;
    }

    true
  }

  pub fn len(&self) -> Option<usize> {
    if let Self::List(list) = self {
      Some(list.len())
    } else {
      None
    }
  }

  pub fn is_empty(&self) -> Option<bool> {
    if let Self::List(list) = self {
      Some(list.is_empty())
    } else {
      None
    }
  }

  pub fn index(&'_ self, idx: usize) -> Option<&'_ Self> {
    if let Self::List(list) = self {
      Some(&list[idx])
    } else {
      None
    }
  }

  pub fn index_mut(&'_ mut self, idx: usize) -> Option<&'_ mut Self> {
    if let Self::List(list) = self {
      Some(&mut list[idx])
    } else {
      None
    }
  }
}

impl New<bool> for Value {
  fn new(item: bool) -> Self {
    Self::Bool(item)
  }
}

impl New<f64> for Value {
  fn new(item: f64) -> Self {
    Self::Num(item)
  }
}

impl New<u128> for Value {
  fn new(item: u128) -> Self {
    Self::U128(item)
  }
}

impl New<usize> for Value {
  fn new(item: usize) -> Self {
    Self::new(item as f64)
  }
}

impl New<i64> for Value {
  fn new(item: i64) -> Self {
    Self::new(item as f64)
  }
}

impl New<i32> for Value {
  fn new(item: i32) -> Self {
    Self::new(item as f64)
  }
}

impl New<String> for Value {
  fn new(item: String) -> Self {
    Self::Str(item)
  }
}

impl New<&str> for Value {
  fn new(item: &str) -> Self {
    Self::new(String::from(item))
  }
}

impl New<Values> for Value {
  fn new(item: Values) -> Self {
    Self::List(item)
  }
}

impl New<Function> for Value {
  fn new(item: Function) -> Self {
    Self::Function(item)
  }
}

impl<F: FnMut(&[Value]) -> ValueOpResult + 'static> New<F> for Value {
  fn new(item: F) -> Self {
    Self::NativeFunction(SmartPtr::new(Box::new(item)))
  }
}

pub type ValueOpResult = Result<Value, String>;

impl Add for Value {
  type Output = ValueOpResult;
  fn add(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a + b)),
        Self::Str(b) => Ok(Self::Str(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => Ok(Self::Str(format!("{}{}", a, b))),
        Self::Str(b) => Ok(Self::Str(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      _ => Err(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl Sub for Value {
  type Output = ValueOpResult;
  fn sub(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a - b)),
        _ => Err(format!("cannot sub {} and {}", a, other)),
      },
      _ => Err(format!("cannot sub {} and {}", self, other)),
    }
  }
}

impl Mul for Value {
  type Output = ValueOpResult;
  fn mul(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a * b)),
        Self::Str(b) => {
          if a > 0.0 {
            Ok(Self::Str(b.repeat(a as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", b))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => {
          if b > 0.0 {
            Ok(Self::new(a.repeat(b as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", a))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      _ => Err(format!("cannot multiply {} and {}", self, other)),
    }
  }
}

impl Div for Value {
  type Output = ValueOpResult;
  fn div(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a / b)),
        _ => Err(format!("cannot divide {} by {}", self, other)),
      },
      _ => Err(format!("cannot divide {} by {}", self, other)),
    }
  }
}

impl Rem for Value {
  type Output = ValueOpResult;
  fn rem(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a % b)),
        _ => Err(format!("cannot modulus {} by {}", self, other)),
      },
      _ => Err(format!("cannot modulus {} by {}", self, other)),
    }
  }
}

impl Not for Value {
  type Output = Self;
  fn not(self) -> Self::Output {
    Value::Bool(!self.truthy())
  }
}

impl Neg for Value {
  type Output = ValueOpResult;
  fn neg(self) -> Self::Output {
    match self {
      Self::Num(n) => Ok(Self::Num(-n)),
      _ => Err(format!("cannot negate '{}'", self)),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match self {
      Self::Bool(a) => {
        if let Self::Bool(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Str(a) => {
        if let Self::Str(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Num(a) => {
        if let Self::Num(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::U128(a) => {
        if let Self::U128(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::List(a) => {
        if let Self::List(b) = other {
          let a = &a.0;
          let b = &b.0;

          if a.len() == b.len() {
            for it in a.iter().zip(b.iter()) {
              let (ai, bi) = it;
              if ai != bi {
                return false;
              }
            }

            true
          } else {
            false
          }
        } else {
          false
        }
      }
      Self::Nil => {
        matches!(other, Value::Nil)
      }
      Self::Function(a) => {
        if let Self::Function(b) = other {
          a.ctx.id == b.ctx.id
        } else {
          false
        }
      }
      Self::NativeFunction(a) => {
        if let Self::NativeFunction(b) = other {
          std::ptr::addr_of!(**a) == std::ptr::addr_of!(**b)
        } else {
          false
        }
      }
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => {
          if a < b {
            Some(Ordering::Less)
          } else if a > b {
            Some(Ordering::Greater)
          } else if (a - b).abs() < f64::EPSILON {
            Some(Ordering::Equal)
          } else {
            None
          }
        }
        _ => None,
      },
      Self::U128(a) => match other {
        Self::U128(b) => Some(a.cmp(b)),
        _ => None,
      },
      Self::Str(a) => match other {
        Self::Str(b) => Some(a.cmp(b)),
        _ => None,
      },
      _ => None,
    }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nil => write!(f, "nil"),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Num(n) => write!(f, "{}", n),
      Self::U128(n) => write!(f, "{}", n),
      Self::Str(s) => write!(f, "{}", s),
      Self::List(l) => write!(f, "{}", l),
      Self::Function(func) => write!(f, "<function {}>", func.name),
      Self::NativeFunction(func) => write!(f, "<function @{:p}>", func),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

#[derive(Clone)]
pub struct Values(Vec<Value>);

impl Values {
  pub fn new(values: Vec<Value>) -> Self {
    Self(values)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

impl Index<usize> for Values {
  type Output = Value;

  fn index(&self, idx: usize) -> &Self::Output {
    &self.0[idx]
  }
}

impl IndexMut<usize> for Values {
  fn index_mut(&mut self, idx: usize) -> &mut Value {
    &mut self.0[idx]
  }
}

impl IntoIterator for Values {
  type Item = Value;
  type IntoIter = std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl Display for Values {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.iter().fold(Ok(()), |result, value| {
      result.and_then(|_| writeln!(f, "{}", value))
    })
  }
}

#[derive(Default)]
pub struct Env {
  vars: BTreeMap<String, Value>,
}

impl Env {
  pub fn define(&mut self, name: String, value: Value) -> bool {
    self.vars.insert(name, value).is_none()
  }

  pub fn assign(&mut self, name: String, value: Value) -> bool {
    self.vars.insert(name, value).is_some()
  }

  pub fn lookup(&self, name: &str) -> Option<Value> {
    self.vars.get(name).cloned()
  }
}

pub trait Call {
  fn call(&mut self, args: Vec<Value>) -> ValueOpResult;
}

#[derive(Clone)]
pub struct Function {
  name: String,
  airity: usize,
  ctx: SmartPtr<Context>,
}

impl Function {
  pub fn new(name: String, airity: usize, ctx: SmartPtr<Context>) -> Self {
    Self { name, airity, ctx }
  }

  pub fn call<I: Interpreter>(&mut self, i: &I, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() > self.airity {
      return Err(format!(
        "too many arguments number of arguments, expected {}, got {}",
        self.airity,
        args.len()
      ));
    }

    while args.len() < self.airity {
      args.push(Value::Nil);
    }

    let prev_ip = self.ctx.ip;
    self.ctx.ip = 0;
    let prev_stack = self.ctx.stack_move(args);

    let res = i.interpret(&mut self.ctx).map_err(|e| e.msg);

    self.ctx.ip = prev_ip;
    self.ctx.stack_move(prev_stack);

    res
  }

  #[cfg(debug_assertions)]
  pub fn context(&self) -> &Context {
    &self.ctx
  }
}

#[cfg(test)]
mod test;
