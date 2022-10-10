use ptr::SmartPtr;

use crate::{Args, Context, ExecutionThread};

use super::{ComplexValue, FunctionValue};

#[derive(Clone)]
pub struct MethodValue {
  pub function: FunctionValue,
}

impl MethodValue {
  pub fn new(function: FunctionValue) -> Self {
    Self { function }
  }

  pub fn call(&self, thread: &mut ExecutionThread, args: Args) {
    let mut new_args = Vec::with_capacity(1 + args.list.len());
    new_args.push(args.this.unwrap());
    new_args.extend(args.list);
    self.function.call(thread, Args::from(new_args));
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    self.function.context_ptr()
  }

  pub fn context(&self) -> &Context {
    self.function.context()
  }

  pub fn context_mut(&mut self) -> &mut Context {
    self.function.context_mut()
  }
}

impl ComplexValue for MethodValue {}
