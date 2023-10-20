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
use std::{convert::Infallible, error::Error, fmt::Debug, vec::IntoIter};
pub use string_value::StringValue;
pub use struct_value::StructValue;
use thiserror::Error;
pub use timestamp_value::TimestampValue;
use uuid::Uuid;

pub struct Nil;

pub trait Usertype
where
  Self: UsertypeFields + UsertypeMethods + DisplayValue + DebugValue + LockableValue + TraceableValue + Sized + 'static,
{
  const ID: Uuid;
  const VTABLE: VTable = VTable::new::<Self>();

  fn get(&self, gc: &mut Gc, this: &Value, field: &str) -> ValueResult {
    match <Self as UsertypeFields>::get_field(self, gc, field) {
      Ok(Some(value)) => Ok(value),
      Ok(None) => match <Self as UsertypeMethods>::get_method(self, gc, this, field) {
        Ok(Some(value)) => Ok(value),
        Ok(None) => Ok(Value::nil),
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  fn set(&mut self, gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
    <Self as UsertypeFields>::set_field(self, gc, field, value)
  }
}

pub trait UsertypeFields {
  fn get_field(&self, gc: &mut Gc, field: &str) -> ValueResult<Option<Value>>;
  fn set_field(&mut self, gc: &mut Gc, field: &str, value: Value) -> ValueResult<()>;
}

pub trait UsertypeMethods {
  fn __new__(_gc: &mut Gc, _args: Args) -> ValueResult {
    Err(ValueError::UndefinedInitializer)
  }
  fn get_method(&self, gc: &mut Gc, this: &Value, field: &str) -> ValueResult<Option<Value>>;
}

pub trait DisplayValue {
  fn __str__(&self) -> String;
}

pub trait DebugValue {
  fn __dbg__(&self) -> String;
}

pub trait LockableValue {
  fn __lock__(&mut self) {}
}

pub trait TraceableValue {
  fn trace(&self, marks: &mut Marker);
}
pub struct Args {
  pub list: Vec<Value>,
}

impl Args {
  pub fn new(args: impl Into<Vec<Value>>) -> Self {
    Self { list: args.into() }
  }

  pub fn new_with_this(this: Value, args: impl Into<Vec<Value>>) -> Self {
    let mut args = args.into();
    args.push(this);
    Self { list: args }
  }

  pub fn count(&self) -> usize {
    self.list.len()
  }

  pub fn into_iter(self) -> ArgIter {
    ArgIter {
      items: self.list.into_iter(),
    }
  }
}

pub struct ArgIter {
  items: IntoIter<Value>,
}

impl<'vm> ArgIter {
  pub fn next_arg(&'vm mut self) -> Arg<'vm> {
    Arg { items: &mut self.items }
  }
}

pub struct Arg<'iter> {
  items: &'iter mut IntoIter<Value>,
}

pub trait TryUnwrapArg<T> {
  fn try_unwrap_arg(&mut self, fn_name: &'static str, pos: usize) -> ValueResult<T>;
}

impl<T> TryUnwrapArg<T> for Arg<'_>
where
  T: MaybeFrom<Value>,
{
  fn try_unwrap_arg(&mut self, fn_name: &'static str, pos: usize) -> ValueResult<T> {
    T::maybe_from(self.items.next().clone().unwrap()).ok_or(ValueError::InvalidArgument(fn_name, pos))
  }
}

/// Intentionally empty
///
/// bool, char, i32, f64
#[derive(Default, Usertype, Fields)]
#[uuid("6d9d039a-9803-41ff-8e84-a0ea830e2380")]
pub struct Primitive {}

#[methods]
impl Primitive {}

impl TraceableValue for Primitive {
  fn trace(&self, _marks: &mut Marker) {
    // do nothing
  }
}

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
  /// ident
  #[error("Cannot modify immutable object '{0}'")]
  Immutable(String),
  /// ident
  #[error("Tried to access undefined member '{0}'")]
  UndefinedMember(String),

  /// message
  #[error("{0}")]
  RuntimeError(String),

  /// Default return value for usertype initializers
  #[error("Undefined initializer reached")]
  UndefinedInitializer,

  /// meant to be a placeholder for me being lazy
  #[error("{0}")]
  Todo(String),

  #[error("Infallible")]
  Infallible,
}

impl ValueError {
  pub fn runtime_error(msg: impl Into<String>) -> Self {
    Self::RuntimeError(msg.into())
  }
}

impl From<Infallible> for ValueError {
  fn from(_: Infallible) -> Self {
    Self::Infallible
  }
}

pub type ValueResult<T = Value> = Result<T, ValueError>;
