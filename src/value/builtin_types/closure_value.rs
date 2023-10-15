use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields)]
#[uuid("e924d375-93f0-4ce9-a2f3-5a4cf612492e")]
pub struct ClosureValue {
  captures: Vec<Value>,
  function: FunctionValue,
}

impl ClosureValue {
  pub fn new(captures: &[Value], function: FunctionValue) -> Self {
    Self {
      captures: captures.into(),
      function,
    }
  }

  pub fn call(&self, vm: &mut Vm, mut args: Vec<Value>) {
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

    vm.new_frame(self.function.context_ptr().clone());

    vm.set_stack(captures_with_args);
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    self.function.context_ptr()
  }

  pub fn context(&self) -> &Context {
    self.function.context()
  }
}

#[methods]
impl ClosureValue {
  fn __str__(&self) -> String {
    format!("closure {}", self.function.__str__())
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
