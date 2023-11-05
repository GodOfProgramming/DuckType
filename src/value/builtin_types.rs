pub(crate) mod class_value;
pub(crate) mod closure_value;
pub(crate) mod function_value;
pub(crate) mod instance_value;
pub(crate) mod method_value;
pub(crate) mod module_value;
pub(crate) mod native_value;
pub(crate) mod string_value;
pub(crate) mod struct_value;
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
use crate::prelude::*;
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
use macros::{methods, Fields};
pub use method_value::MethodValue;
pub use module_value::{ModuleBuilder, ModuleValue};
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::{
  collections::{BTreeMap, HashMap},
  error::Error,
  vec::IntoIter,
};
pub use string_value::StringValue;
pub use struct_value::StructValue;
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

  fn get(&self, gc: &mut Gc, this: &Value, field: &str) -> UsageResult<Option<Value>> {
    match <Self as UsertypeFields>::get_field(self, gc, field)? {
      Some(value) => Ok(Some(value)),
      None => <Self as UsertypeMethods>::get_method(self, gc, this, field),
    }
  }

  fn set(&mut self, gc: &mut Gc, field: &str, value: Value) -> UsageResult<()> {
    <Self as UsertypeFields>::set_field(self, gc, field, value)
  }
}

pub trait UsertypeFields {
  fn get_field(&self, gc: &mut Gc, field: &str) -> UsageResult<Option<Value>>;
  fn set_field(&mut self, gc: &mut Gc, field: &str, value: Value) -> UsageResult<()>;
}

pub trait UsertypeMethods {
  fn __new__(_vm: &mut Vm, _args: Args) -> UsageResult {
    Err(UsageError::UndefinedInitializer)
  }
  fn get_method(&self, gc: &mut Gc, this: &Value, field: &str) -> UsageResult<Option<Value>>;
}

pub trait ResolvableValue: DisplayValue {
  #[allow(unused_variables)]
  fn __def__(&mut self, field: &str, value: Value) -> UsageResult<bool> {
    Err(UsageError::TypeError(self.__str__(), String::from("module")))
  }

  #[allow(unused_variables)]
  fn __res__(&self, field: &str) -> UsageResult {
    Err(UsageError::TypeError(self.__str__(), String::from("module")))
  }
}

pub trait InvocableValue {
  #[allow(unused_variables)]
  fn __ivk__(&mut self, vm: &mut Vm, this: Value, args: Args) -> UsageResult {
    Err(UsageError::UndefinedMethod("__ivk__"))
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

impl<T, S> TraceableValue for HashMap<T, Value, S> {
  fn trace(&self, marks: &mut Marker) {
    for value in self.values() {
      marks.trace(value);
    }
  }
}

impl<T, V, S> TraceableValue for HashMap<T, V, S>
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

#[derive(Default)]
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
  fn try_unwrap_arg(&mut self, fn_name: &'static str, pos: usize) -> UsageResult<T>;
}

impl<T> TryUnwrapArg<T> for Arg<'_>
where
  T: MaybeFrom<Value>,
{
  fn try_unwrap_arg(&mut self, fn_name: &'static str, pos: usize) -> UsageResult<T> {
    T::maybe_from(self.items.next().clone().unwrap()).ok_or(UsageError::InvalidArgument(fn_name, pos))
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

pub(crate) trait ConsumeResult<T> {
  fn consume<F, O>(self, f: F) -> UsageResult<O>
  where
    F: FnOnce(T) -> O;
}

impl<T> ConsumeResult<T> for UsageResult<T> {
  fn consume<F, O>(self, f: F) -> UsageResult<O>
  where
    F: FnOnce(T) -> O,
  {
    Ok(f(self?))
  }
}
