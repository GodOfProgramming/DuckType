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
  pub const LESS_EQUAL: &str = "__leq__";
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
use macros::Class;
pub use method_value::MethodValue;
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::{collections::BTreeMap, convert::Infallible, error::Error};
pub use string_value::StringValue;
pub use struct_value::StructValue;
use thiserror::Error;
pub use timestamp_value::TimestampValue;

pub struct Nil;

pub trait Usertype
where
  Self: Class + Sized + 'static,
{
  const ID: &'static str;
  const VTABLE: VTable = VTable::new::<Self>();

  fn stringify(&self) -> String {
    Self::ID.to_string()
  }

  fn debug_string(&self) -> String {
    self.stringify()
  }
}

pub trait Class {
  fn id(&self) -> &'static str;
  fn get(&self, field: &str) -> Option<Value>;
  fn set(&mut self, field: &str, value: Value) -> ValueResult<()>;
}

pub trait ClassBody: Class {
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

  pub(crate) fn construct(&self, this_class: Value, vm: &mut Vm, env: &mut Env, mut args: Args) -> ValueResult {
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
#[derive(Class)]
pub struct Primitive {}

impl Usertype for Primitive {
  const ID: &'static str = "Primitive";
}

impl TryFrom<Value> for Primitive {
  type Error = Box<dyn Error>;

  fn try_from(value: Value) -> Result<Self, Self::Error> {
    unimplemented!()
  }
}

// TODO fill out this error
#[derive(Debug, Error)]
pub enum ValueError {
  /// fn name
  #[error("MissingSelf: missing self in call to {0}")]
  MissingSelf(&'static str),
  /// fn name, argument index, error
  #[error("InvalidArgument: wrong type passed to {0} in argument position {1}")]
  InvalidArgument(&'static str, usize),
  /// fn name, type, actual/this
  #[error("BadCast: wrong cast in {0} (casting from {2} to {1})")]
  BadCast(&'static str, &'static str, Value),
  /// given, expected
  #[error("ArgumentError: wrong number of arguments (given {0} expected {1})")]
  ArgumentError(usize, usize),
  /// tried, needed
  #[error("CoercionError: cannot coerce {0} to {1}")]
  CoercionError(Value, &'static str),
  /// op, lhs, rhs
  #[error("Tried to perform '{0}' with {1} and {2}")]
  InvalidOperation(char, Value, Value),
  /// method, value
  #[error("{0} not implemented for {1}")]
  UnimplementedError(&'static str, Value),
  /// member name
  #[error("Tried assigning a value to unimplemented member {0}")]
  InvalidAssignment(String),
  /// meant to be a placeholder for me being lazy
  #[error("{0}")]
  Todo(String),

  #[error("Infallible")]
  Infallible,
}

impl From<Infallible> for ValueError {
  fn from(_: Infallible) -> Self {
    Self::Infallible
  }
}

pub type ValueResult<T = Value> = Result<T, ValueError>;
