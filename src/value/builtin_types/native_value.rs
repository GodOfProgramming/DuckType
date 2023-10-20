use super::Args;
use crate::prelude::*;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub type NativeFn = for<'a> fn(&mut Vm, Args) -> ValueResult;

type NativeClosureType = dyn FnMut(&mut Vm, Args) -> ValueResult;

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
    F: FnMut(&mut Vm, Args) -> ValueResult + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, vm: &mut Vm, args: Args) -> ValueResult {
    (*self.callee)(vm, args)
  }
}

#[methods]
impl NativeClosureValue {
  fn __str__(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeClosureValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "<native {} @{:p}>", self.name, self.callee.as_ref())
  }
}

impl TraceableValue for NativeClosureValue {
  fn trace(&self, _marks: &mut Marker) {
    // do nothing
  }
}

pub enum NativeCallable {
  NativeFn(NativeFn),
  NativeClosure(NativeClosureValue),
}

impl NativeCallable {
  pub fn call(&mut self, vm: &mut Vm, args: Args) -> ValueResult {
    match self {
      NativeCallable::NativeFn(f) => f(vm, args),
      NativeCallable::NativeClosure(c) => c.call(vm, args),
    }
  }
}

impl Display for NativeCallable {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      NativeCallable::NativeFn(nf) => write!(f, "{:p}", &nf),
      NativeCallable::NativeClosure(c) => write!(f, "{}", c),
    }
  }
}

#[derive(Usertype, Fields)]
#[uuid("846eb503-820d-446a-9b54-7a274d85cd32")]
pub struct NativeMethodValue {
  pub this: Value,
  callee: NativeCallable,
}

impl NativeMethodValue {
  pub fn new_native_fn(this: Value, callee: NativeFn) -> Self {
    Self {
      this,
      callee: NativeCallable::NativeFn(callee),
    }
  }

  pub fn new_native_closure(this: Value, callee: NativeClosureValue) -> Self {
    Self {
      this,
      callee: NativeCallable::NativeClosure(callee),
    }
  }

  pub fn call(&mut self, vm: &mut Vm, args: Args) -> ValueResult {
    self.callee.call(vm, args)
  }
}

#[methods]
impl NativeMethodValue {
  fn __str__(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeMethodValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.callee)
  }
}

impl TraceableValue for NativeMethodValue {
  fn trace(&self, marks: &mut Marker) {
    marks.trace(&self.this);
  }
}
