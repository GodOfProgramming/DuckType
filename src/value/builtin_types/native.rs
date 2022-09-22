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

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Vec<Value>) -> Value {
    (*self.callee)(thread, env, args)
  }
}

impl Object for NativeFn {}

impl Display for NativeFn {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "<native {}>", self.name)
  }
}

// type NativeMethodTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static;
pub struct NativeMethod {
  this: Value,
  callee: Value,
}

impl NativeMethod {
  pub fn new(this: Value, f: F) -> Self {
    Self { this, callee: f }
  }

  pub fn call(
    &mut self,
    thread: &mut ExecutionThread,
    env: &mut Env,
    mut args: Vec<Value>,
  ) -> Value {
    args.push(self.this.clone());
    (*self.callee)(thread, env, args)
  }
}

impl Object for NativeMethod {}

impl Display for NativeMethod {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", self.callee)
  }
}
