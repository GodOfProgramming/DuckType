use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields, NoMethods)]
#[uuid("e924d375-93f0-4ce9-a2f3-5a4cf612492e")]
pub struct ClosureValue {
  #[trace]
  captures: Vec<Value>,
  #[trace]
  function: FunctionValue,
}

impl ClosureValue {
  pub fn new(captures: &[Value], function: FunctionValue) -> Self {
    Self {
      captures: captures.into(),
      function,
    }
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    self.function.context_ptr()
  }

  pub fn context(&self) -> &Context {
    self.function.context()
  }
}

impl Operators for ClosureValue {
  fn __ivk__(&mut self, vm: &mut Vm, _this_fn: Value, airity: usize) -> UsageResult {
    self.function.check_args(airity)?;

    for capture in &self.captures {
      vm.stack_push(capture.clone());
    }

    self.function.invoke(vm, self.captures.len())
  }

  fn __str__(&self) -> String {
    format!("closure {}", self.function.__str__())
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
