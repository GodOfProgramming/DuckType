use super::{Function, Value};
use crate::{Context, ExecutionThread};
use ptr::SmartPtr;

#[derive(Clone)]
pub struct Closure {
  captures: Vec<Value>,
  function: Function,
}

impl Closure {
  pub fn new(captures: &[Value], function: Function) -> Self {
    Self {
      captures: captures.into(),
      function,
    }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, mut args: Vec<Value>) {
    if args.len() > self.function.airity {
      args.drain(0..self.function.airity);
    } else {
      while args.len() < self.function.airity {
        args.push(Value::nil);
      }
    }

    let mut captures_with_args = Vec::with_capacity(self.captures.len() + args.len());
    captures_with_args.extend(self.captures.clone());
    captures_with_args.extend(args);

    thread.new_frame(self.function.context_ptr().clone());

    thread.set_stack(captures_with_args);
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.function.context_ptr()
  }

  pub fn context(&self) -> &Context {
    &self.function.context()
  }
}
