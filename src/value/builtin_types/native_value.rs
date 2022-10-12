use super::{Args, Usertype, UsertypeId, Value};
use crate::{Env, ExecutionThread};
use std::fmt::{Display, Formatter, Result};

// type NativeFnTrait = FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static;

pub type NativeFn = fn(&mut ExecutionThread, &mut Env, Args) -> Value;

type NativeClosureType = dyn FnMut(&mut ExecutionThread, &mut Env, Args) -> Value;

pub struct NativeClosureValue {
  pub name: String,
  pub callee: Box<NativeClosureType>,
}

impl NativeClosureValue {
  pub fn new<T, F>(name: T, callee: F) -> Self
  where
    T: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    Self {
      name: name.to_string(),
      callee: Box::new(callee),
    }
  }

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) -> Value {
    (*self.callee)(thread, env, args)
  }
}

impl Usertype for NativeClosureValue {
  const ID: UsertypeId = "NativeClosure";

  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeClosureValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "<native {} @{:p}>", self.name, self.callee.as_ref())
  }
}

// type NativeMethodTrait = FnMut(&mut ExecutionThread, &mut Env, Value, Args) -> Value + 'static;
// type NativeMethodType = dyn FnMut(&mut ExecutionThread, &mut Env, Value, Args) -> Value;

pub enum NativeCallable {
  NativeFn(NativeFn),
  NativeClosure(NativeClosureValue),
}

impl NativeCallable {
  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) -> Value {
    match self {
      NativeCallable::NativeFn(f) => f(thread, env, args),
      NativeCallable::NativeClosure(c) => c.call(thread, env, args),
    }
  }
}

impl Display for NativeCallable {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

  pub fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Args) -> Value {
    self.callee.call(thread, env, args)
  }
}

impl Usertype for NativeMethodValue {
  const ID: UsertypeId = "NativeMethod";

  fn stringify(&self) -> String {
    format!("{}", self)
  }
}

impl Display for NativeMethodValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
