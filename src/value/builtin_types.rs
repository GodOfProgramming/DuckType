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
pub(crate) mod vec_value;

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
  pub const INDEX_ASSIGN: &str = "__idxeq__";
}

use super::{VTable, Value};
use crate::{dbg::RuntimeErrors, prelude::*};
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
use macros::{methods, Fields};
pub use method_value::MethodValue;
pub use module_value::{ModuleBuilder, ModuleValue};
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use ptr::SmartPtr;
use std::{
  collections::{BTreeMap, HashMap},
  convert::Infallible,
  error::Error,
  fmt::Debug,
  io,
  sync::mpsc,
  vec::IntoIter,
};
pub use string_value::StringValue;
pub use struct_value::StructValue;
use thiserror::Error;
pub use timestamp_value::TimestampValue;
use uuid::Uuid;
pub use vec_value::VecValue;

pub struct Nil;

pub trait Usertype
where
  Self: UsertypeFields
    + UsertypeMethods
    + InvocableValue
    + ResolvableValue
    + DisplayValue
    + DebugValue
    + TraceableValue
    + Sized
    + 'static,
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
  fn __new__(_vm: &mut Vm, _args: Args) -> ValueResult {
    Err(ValueError::UndefinedInitializer)
  }
  fn get_method(&self, gc: &mut Gc, this: &Value, field: &str) -> ValueResult<Option<Value>>;
}

pub trait ResolvableValue: DisplayValue {
  #[allow(unused_variables)]
  fn __def__(&mut self, field: &str, value: Value) -> ValueResult<bool> {
    Err(ValueError::TypeError(self.__str__(), String::from("module")))
  }

  #[allow(unused_variables)]
  fn __res__(&self, field: &str) -> ValueResult {
    Err(ValueError::TypeError(self.__str__(), String::from("module")))
  }
}

pub trait InvocableValue {
  #[allow(unused_variables)]
  fn __ivk__(&mut self, vm: &mut Vm, this: Value, args: Args) -> ValueResult<()> {
    Err(ValueError::UndefinedMethod("__ivk__"))
  }
}

pub trait DisplayValue {
  fn __str__(&self) -> String;
}

pub trait DebugValue {
  fn __dbg__(&self) -> String;
}

pub trait TraceableValue {
  #[allow(unused_variables)]
  fn trace(&self, marks: &mut Marker);
}

impl TraceableValue for SmartPtr<Context> {
  fn trace(&self, marks: &mut Marker) {
    self.trace_all(marks);
  }
}

impl TraceableValue for Option<Value> {
  fn trace(&self, marks: &mut Marker) {
    if let Some(value) = self {
      marks.trace(value);
    }
  }
}

impl TraceableValue for Vec<Value> {
  fn trace(&self, marks: &mut Marker) {
    for value in self {
      marks.trace(value);
    }
  }
}

impl<T> TraceableValue for HashMap<T, Value> {
  fn trace(&self, marks: &mut Marker) {
    for value in self.values() {
      marks.trace(value);
    }
  }
}

impl<T, V> TraceableValue for HashMap<T, V>
where
  V: TraceableValue,
{
  fn trace(&self, marks: &mut Marker) {
    for value in self.values() {
      value.trace(marks);
    }
  }
}

impl<T> TraceableValue for BTreeMap<T, Value> {
  fn trace(&self, marks: &mut Marker) {
    for value in self.values() {
      marks.trace(value);
    }
  }
}

impl<T, V> TraceableValue for BTreeMap<T, V>
where
  V: TraceableValue,
{
  fn trace(&self, marks: &mut Marker) {
    for value in self.values() {
      value.trace(marks);
    }
  }
}

pub struct Args {
  pub list: Vec<Value>,
}

impl Args {
  pub fn new(args: impl Into<Vec<Value>>) -> Self {
    Self { list: args.into() }
  }

  pub fn new_with_this(this: Value, args: impl Into<Vec<Value>>) -> Self {
    let args = args.into();
    let mut new_args = Vec::with_capacity(1 + args.len());
    new_args.push(this);
    new_args.extend(args);
    Self { list: new_args }
  }

  pub fn count(&self) -> usize {
    self.list.len()
  }

  pub fn into_arg_iter(self) -> ArgIter {
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
  /// fn name, argument index
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
  #[error("{0} is undefined")]
  UndefinedMethod(&'static str),
  /// index, value
  #[error("Index {0} out of bounds in {1}")]
  InvalidIndex(i32, Value),
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
  RuntimeError(RuntimeErrors),

  /// actual, expected
  #[error("{0} is not a {1}")]
  TypeError(String, String),

  /// ident
  #[error("use of undefined variable {0}")]
  NameError(String),

  /// Native function produced an error
  /// fn name
  #[error("error in native function: {0}")]
  NativeApi(String),

  /// Default return value for usertype initializers
  #[error("Undefined initializer reached")]
  UndefinedInitializer,

  /// std::io error
  #[error("{0}")]
  IoError(std::io::Error),

  /// Gc sender error
  #[error("Garbage Collector Failure: {0}")]
  GcError(Box<dyn Error>),

  /// Can only be reached from a bug
  #[error("Infallible")]
  Infallible,

  /// meant to be a placeholder for me being lazy
  #[error("{0}")]
  Todo(String),
}

impl From<Infallible> for ValueError {
  fn from(_: Infallible) -> Self {
    Self::Infallible
  }
}

impl From<io::Error> for ValueError {
  fn from(value: io::Error) -> Self {
    Self::IoError(value)
  }
}

impl From<RuntimeErrors> for ValueError {
  fn from(value: RuntimeErrors) -> Self {
    Self::RuntimeError(value)
  }
}

impl<T> From<mpsc::SendError<T>> for ValueError {
  fn from(value: mpsc::SendError<T>) -> Self {
    Self::GcError(value.to_string().into())
  }
}

pub type ValueResult<T = Value> = Result<T, ValueError>;

pub(crate) trait ConsumeResult<T> {
  fn consume<F, O>(self, f: F) -> ValueResult<O>
  where
    F: FnOnce(T) -> O;
}

impl<T> ConsumeResult<T> for ValueResult<T> {
  fn consume<F, O>(self, f: F) -> ValueResult<O>
  where
    F: FnOnce(T) -> O,
  {
    Ok(f(self?))
  }
}
