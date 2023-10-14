use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone, Usertype, Class)]
pub struct MethodValue {
  pub this: Value,
  pub function: FunctionValue,
}

impl MethodValue {
  pub fn new(this: Value, function: FunctionValue) -> Self {
    Self { this, function }
  }

  pub fn call(&self, vm: &mut Vm, args: Args) {
    self.function.call(vm, args.list);
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

#[methods]
impl MethodValue {
  fn __str__(&self) -> String {
    format!("method {}", self.function.__str__())
  }

  fn debug_string(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
