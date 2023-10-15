pub(crate) mod array_value;
pub(crate) mod class_value;
pub(crate) mod closure_value;
pub(crate) mod function_value;
pub(crate) mod instance_value;
pub(crate) mod method_value;
pub(crate) mod module_value;
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

  pub const EQUALITY: &str = "__eq__";
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
use macros::{methods, Fields};
pub use method_value::MethodValue;
pub use module_value::{LockedModule, ModuleValue};
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::{convert::Infallible, error::Error, fmt::Debug};
pub use string_value::StringValue;
pub use struct_value::StructValue;
use thiserror::Error;
pub use timestamp_value::TimestampValue;
use uuid::Uuid;

pub struct Nil;

pub trait Usertype
where
  Self: UsertypeFields + UsertypeMethods + DisplayValue + DebugValue + LockableValue + Sized + 'static,
{
  const ID: Uuid;
  const VTABLE: VTable = VTable::new::<Self>();

  fn get(&self, this: &Value, field: &str) -> ValueResult<Value> {
    Ok(
      <Self as UsertypeFields>::get_field(self, field)
        .or_else(|| <Self as UsertypeMethods>::get_method(self, this, field))
        .unwrap_or_default(),
    )
  }

  fn set(&mut self, field: &str, value: Value) -> ValueResult<()> {
    <Self as UsertypeFields>::set_field(self, field, value)
  }
}

pub trait UsertypeFields {
  fn get_field(&self, field: &str) -> Option<Value>;
  fn set_field(&mut self, field: &str, value: Value) -> ValueResult<()>;
}

pub trait UsertypeMethods {
  fn get_method(&self, this: &Value, field: &str) -> Option<Value>;
}

pub trait LockableValue {
  fn __lock__(&mut self) {}
}

pub trait DisplayValue {
  fn __str__(&self) -> String;
}

pub trait DebugValue {
  fn __dbg__(&self) -> String;
}

#[derive(Default, Debug)]
pub struct Args {
  pub list: Vec<Value>,
}

impl Args {
  pub fn new_with_this(this: Value, mut args: Vec<Value>) -> Self {
    args.push(this);
    Self { list: args }
  }
  pub fn count(&self) -> usize {
    self.list.len()
  }
}

impl From<Value> for Args {
  fn from(arg: Value) -> Self {
    Self { list: vec![arg] }
  }
}

impl From<Vec<Value>> for Args {
  fn from(list: Vec<Value>) -> Self {
    Self { list }
  }
}

impl<T: Into<Value> + Clone, const I: usize> From<[T; I]> for Args {
  fn from(list: [T; I]) -> Self {
    Self {
      list: list.into_iter().map(|v| -> Value { v.into() }).collect(),
    }
  }
}

/// Intentionally empty
#[derive(Usertype, Fields)]
#[uuid("6d9d039a-9803-41ff-8e84-a0ea830e2380")]
pub struct Primitive {}

#[methods]
impl Primitive {}

impl TryFrom<Value> for Primitive {
  type Error = Box<dyn Error>;

  fn try_from(_value: Value) -> Result<Self, Self::Error> {
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
  /// value
  #[error("Tried to lookup a member on a primitive '{0}'")]
  InvalidLookup(Value),
  #[error("Cannot modify immutable object '{0}'")]
  Immutable(String),
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
