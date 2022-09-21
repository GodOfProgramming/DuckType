use std::ops::{Deref, DerefMut};

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

// type NativeMethodTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static;
type NativeMethodType = dyn FnMut(&mut ExecutionThread, &mut Env, Value, Vec<Value>) -> Value;

pub struct NativeMethod {}
