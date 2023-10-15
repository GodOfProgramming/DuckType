use super::Args;
use crate::prelude::*;
use std::fmt::{Display, Formatter, Result as FmtResult};

pub type NativeFn = for<'a, 'b> fn(&'a mut Vm, &'b mut Env, Args) -> ValueResult;

type NativeClosureType = dyn FnMut(&mut Vm, &mut Env, Args) -> ValueResult;

#[derive(Usertype, Class)]
pub struct NativeClosureValue {
  pub name: String,
  pub callee: Box<NativeClosureType>,
}

impl NativeClosureValue {
  pub fn new<T, F>(name: T, callee: F) -> Self
  where
    T: ToString,
    F: FnMut(&mut Vm, &mut Env, Args) -> ValueResult + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> ValueResult {
    (*self.callee)(vm, env, args)
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

pub enum NativeCallable {
  NativeFn(NativeFn),
  NativeClosure(NativeClosureValue),
}

impl NativeCallable {
  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> ValueResult {
    match self {
      NativeCallable::NativeFn(f) => f(vm, env, args),
      NativeCallable::NativeClosure(c) => c.call(vm, env, args),
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

#[derive(Usertype, Class)]
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

  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> ValueResult {
    self.callee.call(vm, env, args)
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
