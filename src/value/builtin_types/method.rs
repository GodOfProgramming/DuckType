use super::{ComplexValue, ComplexValueId, FunctionValue};
use crate::{Args, Context, ExecutionThread};
use ptr::SmartPtr;

#[derive(Clone)]
pub struct MethodValue {
  pub function: FunctionValue,
}

impl MethodValue {
  pub fn new(function: FunctionValue) -> Self {
    Self { function }
  }

  pub fn call(&self, thread: &mut ExecutionThread, mut args: Args) {
    args.list.push(args.this.unwrap());
    self.function.call(thread, args.list);
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

impl ComplexValue for MethodValue {
  const ID: ComplexValueId = "Method";

  fn stringify(&self) -> String {
    format!("method {}", self.function.stringify())
  }

  fn debug_string(&self) -> String {
    format!("<{}>", self.stringify())
  }
}
