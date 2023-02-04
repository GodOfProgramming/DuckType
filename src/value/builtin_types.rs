pub(crate) mod array_value;
pub(crate) mod class_value;
pub(crate) mod closure_value;
pub(crate) mod function_value;
pub(crate) mod instance_value;
pub(crate) mod method_value;
pub(crate) mod native_value;
pub(crate) mod string_value;
pub(crate) mod struct_value;
pub(crate) mod timestamp_value;

pub mod ops {
  pub const NOT: &str = "__not__";
  pub const NEG: &str = "__neg__";

  pub const ADD: &str = "__add__";
  pub const SUB: &str = "__sub__";
  pub const MUL: &str = "__mul__";
  pub const DIV: &str = "__div__";
  pub const REM: &str = "__rem__";

  pub const EQUALITY: &str = "__equal__";
  pub const NOT_EQUAL: &str = "__neq__";
  pub const LESS: &str = "__less__";
  pub const LESS_EQUAL: &str = "__leq_";
  pub const GREATER: &str = "__greater__";
  pub const GREATER_EQUAL: &str = "__geq__";

  pub const INDEX: &str = "__index__";
}

use super::{VTable, Value};
use crate::prelude::*;
pub use array_value::ArrayValue;
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
pub use method_value::MethodValue;
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::collections::BTreeMap;
pub use string_value::StringValue;
pub use struct_value::StructValue;
use thiserror::Error;
pub use timestamp_value::TimestampValue;

pub struct Nil;

pub trait Usertype
where
  Self: Sized + 'static,
{
  const ID: &'static str;
  const VTABLE: VTable = VTable::new::<Self>();

  fn lookup(&self, name: &str) -> Value {
    Value::nil
  }

  fn assign(&mut self, name: &str, value: Value) {}

  fn stringify(&self) -> String {
    Self::ID.to_string()
  }

  fn debug_string(&self) -> String {
    self.stringify()
  }
}

pub trait Class {
  const ID: &'static str;
  fn get(&self, field: &str) -> Option<Value>;
  fn set(&mut self, field: &str, value: Value) -> Result<(), ValueError>;
}

pub trait ClassBody {
  fn lookup(name: &str) -> Option<Value>;
}

pub struct NativeClass {
  name: &'static str,
  constructor: Option<NativeFn>,
  methods: BTreeMap<String, Value>,
  statics: BTreeMap<String, Value>,

  setters: BTreeMap<String, Box<dyn Fn(&mut Value, Value)>>,
  getters: BTreeMap<String, Box<dyn Fn(&Value) -> Value>>,
}

impl NativeClass {
  pub fn new<T: Usertype>() -> Self {
    Self {
      name: T::ID,
      constructor: None,
      methods: BTreeMap::default(),
      statics: BTreeMap::default(),
      setters: BTreeMap::default(),
      getters: BTreeMap::default(),
    }
  }

  pub fn name(&self) -> &'static str {
    self.name
  }

  pub fn set_for_instance(&self, instance: &mut Value, name: &str, value: Value) {
    self.setters.get(name).map(|s| s(instance, value));
  }

  pub fn get_for_instance(&self, instance: &Value, name: &str) -> Value {
    self
      .getters
      .get(name)
      .map(|g| g(instance))
      .unwrap_or_else(|| self.methods.get(name).cloned().unwrap_or_default())
  }

  pub fn set_static(&mut self, name: impl ToString, value: Value) {
    self.statics.insert(name.to_string(), value);
  }

  pub fn get_static(&self, name: &str) -> Value {
    self.statics.get(name).cloned().unwrap_or_default()
  }

  pub(crate) fn construct(&self, this_class: Value, vm: &mut Vm, env: &mut Env, mut args: Args) -> Result<Value, ValueError> {
    let this = Value::from(InstanceValue::new(Default::default(), this_class));
    if let Some(constructor) = self.constructor {
      args.this = Some(this);
      constructor(vm, env, args)
    } else {
      Ok(this)
    }
  }
}

#[derive(Default, Debug)]
pub struct Args {
  pub this: Option<Value>,
  pub list: Vec<Value>,
}

impl Args {
  pub fn count(&self) -> usize {
    (if self.this.is_some() { 1 } else { 0 }) + self.list.len()
  }
}

impl From<Value> for Args {
  fn from(arg: Value) -> Self {
    Self {
      this: None,
      list: vec![arg],
    }
  }
}

impl From<(Value, Value)> for Args {
  fn from((this, arg): (Value, Value)) -> Self {
    Self {
      this: Some(this),
      list: vec![arg],
    }
  }
}

impl From<Vec<Value>> for Args {
  fn from(list: Vec<Value>) -> Self {
    Self { this: None, list }
  }
}

impl From<(Value, Vec<Value>)> for Args {
  fn from((this, list): (Value, Vec<Value>)) -> Self {
    Self { this: Some(this), list }
  }
}

impl<T: Into<Value> + Clone, const I: usize> From<(Value, [T; I])> for Args {
  fn from((this, list): (Value, [T; I])) -> Self {
    Self {
      this: Some(this),
      list: list.into_iter().map(|v| -> Value { v.into() }).collect(),
    }
  }
}

pub enum UnimplementedFunction {
  Set,
  Get,
  Index,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Neg,
  Not,
  Cmp,
  Custom(String),
}

impl UnimplementedFunction {
  fn fmt<T: Usertype>(&self, v: &T) -> String {
    format!(
      "{} is unimplemented for {}",
      match self {
        UnimplementedFunction::Set => "set",
        UnimplementedFunction::Get => "get",
        UnimplementedFunction::Index => "index",
        UnimplementedFunction::Add => "add",
        UnimplementedFunction::Sub => "sub",
        UnimplementedFunction::Mul => "mul",
        UnimplementedFunction::Div => "div",
        UnimplementedFunction::Rem => "rem",
        UnimplementedFunction::Neg => "neg",
        UnimplementedFunction::Not => "not",
        UnimplementedFunction::Cmp => "cmp",
        UnimplementedFunction::Custom(s) => s,
      },
      v.stringify()
    )
  }
}

/// Intentionally empty
pub struct Primitive;

impl Usertype for Primitive {
  const ID: &'static str = "";
}

// TODO fill out this error
#[derive(Debug, Error)]
pub enum ValueError {
  // fn name
  #[error("missing self in call to {0}")]
  MissingSelf(&'static str),
  // fn name, argument index, error
  #[error("wrong type in {0} arg {1}: {2}")]
  WrongType(&'static str, usize, Box<dyn std::error::Error>),
  // fn name, type, actual/this
  #[error("bad cast in {0} casting from {2} to {1}")]
  BadCast(&'static str, &'static str, Value),
  // given, expected
  #[error("argument error, given {0} expected {1}")]
  ArgumentError(usize, usize),
}
