use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields, NoMethods)]
#[uuid("7d8c2afc-2706-4b79-a7cf-9bdfdc10ab0c")]
pub struct MethodValue {
  #[trace]
  pub this: Value,
  #[trace]
  pub function: FunctionValue,
}

impl MethodValue {
  pub fn new(this: Value, function: FunctionValue) -> Self {
    Self { this, function }
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

impl Operators for MethodValue {
  fn __ivk__(&mut self, vm: &mut Vm, _this_fn: Value, airity: usize) -> UsageResult {
    vm.stack_push(self.this);
    self.function.__ivk__(vm, Value::nil, airity + 1)
  }

  fn __str__(&self) -> String {
    format!("method {}", self.function.__str__())
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
