use super::{Args, Usertype, Value, ValueError};
use crate::prelude::*;
use std::fmt::{Display, Formatter, Result as FmtResult};

// type NativeFnTrait = FnMut(&mut Vm, &mut Env, Args) -> Value + 'static;

pub type NativeFn = fn(&mut Vm, &mut Env, Args) -> Result<Value, ValueError>;

type NativeClosureType = dyn FnMut(&mut Vm, &mut Env, Args) -> Result<Value, ValueError>;

pub struct NativeClosureValue {
  pub name: String,
  pub callee: Box<NativeClosureType>,
}

impl NativeClosureValue {
  pub fn new<T, F>(name: T, callee: F) -> Self
  where
    T: ToString,
    F: FnMut(&mut Vm, &mut Env, Args) -> Result<Value, ValueError> + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> Result<Value, ValueError> {
    (*self.callee)(vm, env, args)
  }
}

impl Usertype for NativeClosureValue {
  const ID: &'static str = "NativeClosure";

  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeClosureValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "<native {} @{:p}>", self.name, self.callee.as_ref())
  }
}

// type NativeMethodTrait = FnMut(&mut Vm, &mut Env, Value, Args) -> Value + 'static;
// type NativeMethodType = dyn FnMut(&mut Vm, &mut Env, Value, Args) -> Value;

pub enum NativeCallable {
  NativeFn(NativeFn),
  NativeClosure(NativeClosureValue),
}

impl NativeCallable {
  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> Result<Value, ValueError> {
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

pub struct NativeMethodValue {
  callee: NativeCallable,
}

impl NativeMethodValue {
  pub fn new_native_fn(callee: NativeFn) -> Self {
    Self {
      callee: NativeCallable::NativeFn(callee),
    }
  }

  pub fn new_native_closure(callee: NativeClosureValue) -> Self {
    Self {
      callee: NativeCallable::NativeClosure(callee),
    }
  }

  pub fn call(&mut self, vm: &mut Vm, env: &mut Env, args: Args) -> Result<Value, ValueError> {
    self.callee.call(vm, env, args)
  }
}

impl Usertype for NativeMethodValue {
  const ID: &'static str = "NativeMethod";

  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeMethodValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.callee)
  }
}

impl From<NativeFn> for NativeMethodValue {
  fn from(f: NativeFn) -> Self {
    Self {
      callee: NativeCallable::NativeFn(f),
    }
  }
}

impl From<NativeClosureValue> for NativeMethodValue {
  fn from(f: NativeClosureValue) -> Self {
    Self {
      callee: NativeCallable::NativeClosure(f),
    }
  }
}
