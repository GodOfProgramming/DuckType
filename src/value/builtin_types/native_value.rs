use super::Args;
use crate::prelude::*;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub type NativeFn = for<'a> fn(&mut Vm, Args) -> UsageResult;

type NativeClosureType = dyn FnMut(&mut Vm, Args) -> UsageResult;

#[derive(Usertype, Fields)]
#[uuid("3c90ac96-ba86-4ceb-9b9a-591af85ca17b")]
pub struct NativeClosureValue {
  pub name: String,
  pub callee: Box<NativeClosureType>,
}

impl NativeClosureValue {
  pub fn new<T, F>(name: T, callee: F) -> Self
  where
    T: ToString,
    F: FnMut(&mut Vm, Args) -> UsageResult + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }
}

#[methods]
impl NativeClosureValue {
  fn __str__(&self) -> String {
    format!("{}", self)
  }

  fn __ivk__(&mut self, vm: &mut Vm, _this: Value, airity: usize) -> UsageResult {
    let args = vm.stack_drain_from(airity);
    (*self.callee)(vm, Args::new(args))
  }
}

impl Display for NativeClosureValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "<native {} @{:p}>", self.name, self.callee.as_ref())
  }
}

#[derive(Usertype, Fields)]
#[uuid("846eb503-820d-446a-9b54-7a274d85cd32")]
pub struct NativeMethodValue {
  #[trace]
  pub this: Value,
  callee: NativeFn,
}

impl NativeMethodValue {
  pub fn new_native_fn(this: Value, callee: NativeFn) -> Self {
    Self { this, callee }
  }
}

#[methods]
impl NativeMethodValue {
  fn __ivk__(&mut self, vm: &mut Vm, _this_method: Value, airity: usize) -> UsageResult {
    let args = vm.stack_drain_from(airity);
    let args = Args::new_with_this(self.this.clone(), args);
    (self.callee)(vm, args)
  }

  fn __str__(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeMethodValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "<native method 0x{:p}>", self.callee)
  }
}
