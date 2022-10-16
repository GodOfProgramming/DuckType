use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone)]
pub struct MethodValue {
  pub function: FunctionValue,
}

impl MethodValue {
  pub fn new(function: FunctionValue) -> Self {
    Self { function }
  }

  pub fn call(&self, vm: &mut Vm, mut args: Args) {
    args.list.push(args.this.unwrap());
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

impl Usertype for MethodValue {
  const ID: UsertypeId = "Method";

  fn stringify(&self) -> String {
    format!("method {}", self.function.stringify())
  }

  fn debug_string(&self) -> String {
    format!("<{}>", self.stringify())
  }
}
