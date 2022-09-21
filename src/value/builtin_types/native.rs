use std::fmt::{Display, Formatter, Result};

use super::{Object, Value};
use crate::{Env, ExecutionThread};

// type NativeFnTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static;
type NativeFnType = dyn FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value;

pub struct NativeFn {
  pub name: String,
  pub callee: Box<NativeFnType>,
}

impl NativeFn {
  pub fn new<F>(name: &str, callee: F) -> Self
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }
}

impl Object for NativeFn {
  fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Vec<Value>) -> Value {
    (*self.callee)(thread, env, args)
  }
}

impl Display for NativeFn {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "<native {}>", self.name)
  }
}

// type NativeMethodTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static;
type NativeMethodType = dyn FnMut(&mut ExecutionThread, &mut Env, Value, Vec<Value>) -> Value;

pub struct NativeMethod {
  name: String,
  callee: Box<NativeMethodType>,
}

impl NativeMethod {
  pub fn new<F: FnMut(&mut ExecutionThread, &mut Env, Value, Vec<Value>) -> Value + 'static>(
    name: &str,
    f: F,
  ) -> Self {
    Self {
      name: name.to_string(),
      callee: Box::new(f),
    }
  }
}

impl Object for NativeMethod {
  fn method_call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    this: Value,
    args: Vec<Value>,
  ) -> Value {
    (*self.callee)(thread, env, this, args)
  }
}

impl Display for NativeMethod {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "<native m {}>", self.name)
  }
}
