use super::{Args, ComplexValue, Value};
use crate::{Env, ExecutionThread};
use std::fmt::{Display, Formatter, Result};

// type NativeFnTrait = FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static;

pub type NativeFn = fn(&mut ExecutionThread, &mut Env, Args) -> Value;

type NativeClosureType = dyn FnMut(&mut ExecutionThread, &mut Env, Args) -> Value;

pub struct NativeClosureValue {
  pub name: String,
  pub callee: Box<NativeClosureType>,
}

impl NativeClosureValue {
  pub fn new<T, F>(name: T, callee: F) -> Self
  where
    T: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) -> Value {
    (*self.callee)(thread, env, args)
  }
}

impl ComplexValue for NativeClosureValue {
  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeClosureValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "<native {} @{:p}>", self.name, self.callee.as_ref())
  }
}

// type NativeMethodTrait = FnMut(&mut ExecutionThread, &mut Env, Value, Args) -> Value + 'static;
// type NativeMethodType = dyn FnMut(&mut ExecutionThread, &mut Env, Value, Args) -> Value;

pub struct NativeMethodValue {
  callee: NativeClosureValue,
}

impl NativeMethodValue {
  pub fn new(callee: NativeClosureValue) -> Self {
    Self { callee }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) -> Value {
    self.callee.call(thread, env, args)
  }
}

impl ComplexValue for NativeMethodValue {
  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeMethodValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", self.callee)
  }
}

impl From<NativeClosureValue> for NativeMethodValue {
  fn from(f: NativeClosureValue) -> Self {
    Self { callee: f }
  }
}
