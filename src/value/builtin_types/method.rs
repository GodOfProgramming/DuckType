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
    self.function.call(thread, args);
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
