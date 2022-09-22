use crate::{Env, ExecutionThread};

use super::{Function, Object, Value};

#[derive(Clone)]
pub struct Method {
  pub this: Value,
  pub function: Function,
}

impl Method {
  pub fn new(this: Value, function: Function) -> Self {
    Self { this, function }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, mut args: Vec<Value>) {
    args.push(self.this.clone());
    self.function.call(thread, env, args);
  }
}

impl Object for Method {}
