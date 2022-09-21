use super::Value;
use crate::{Env, ExecutionThread};

// type NativeFnTrait = FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static;
type NativeFnType = dyn FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value;

pub struct NativeFn {
  pub name: String,
  pub callee: Box<NativeFnType>,
}

impl NativeFn {
  pub fn new<F>(name: String, callee: F) -> Self
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static,
  {
    Self {
      name,
      callee: Box::new(callee),
    }
  }
}
