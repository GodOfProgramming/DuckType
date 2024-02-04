pub(crate) mod class_value;
pub(crate) mod closure_value;
pub(crate) mod function_value;
pub(crate) mod id_value;
pub(crate) mod instance_value;
pub(crate) mod method_value;
pub(crate) mod module_value;
pub(crate) mod native_value;
pub(crate) mod string_value;
pub(crate) mod struct_value;
pub(crate) mod vec_value;

macro_rules! opstr {
  ($op:ident) => {
    concat!("__", stringify!($op), "__")
  };
}
pub mod ops {
  pub const NOT: &str = opstr!(not);
  pub const NEG: &str = opstr!(neg);

  pub const ADD: &str = opstr!(add);
  pub const SUB: &str = opstr!(sub);
  pub const MUL: &str = opstr!(mul);
  pub const DIV: &str = opstr!(div);
  pub const REM: &str = opstr!(rem);

  pub const EQUALITY: &str = opstr!(eq);
  pub const NOT_EQUAL: &str = opstr!(neq);
  pub const LESS: &str = opstr!(less);
  pub const LESS_EQUAL: &str = opstr!(leq);
  pub const GREATER: &str = opstr!(greater);
  pub const GREATER_EQUAL: &str = opstr!(geq);

  pub const INDEX: &str = opstr!(index);
  pub const INDEX_ASSIGN: &str = opstr!(idxeq);

  pub const CALL: &str = opstr!(call);
}

use super::{VTable, Value};
use crate::{code::ConstantValue, prelude::*};
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use function_value::FunctionValue;
pub use id_value::IdValue;
pub use instance_value::InstanceValue;
use macros::{methods, Fields};
pub use method_value::MethodValue;
pub use module_value::{ModuleBuilder, ModuleType, ModuleValue};
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::{
  collections::{BTreeMap, HashMap},
  vec::IntoIter,
};
pub use string_value::StringValue;
pub use struct_value::StructValue;
use uuid::Uuid;
pub use vec_value::VecValue;

#[derive(Clone)]
pub enum Field<'n> {
  Id(usize),
  Named(&'n str),
  NamedId(usize, &'n str),
}

impl<'n> Field<'n> {
  pub fn new(id: usize, name: &'n str) -> Self {
    Self::NamedId(id, name)
  }

  pub fn name(&self, vm: &'n Vm) -> Option<&'n str> {
    match self {
      Field::Id(id) => vm.constant_at(*id).and_then(|c| match c {
        ConstantValue::String(s) => Some(s.as_ref()),
        ConstantValue::StaticString(s) => Some(*s),
        _ => None,
      }),
      Field::Named(name) => Some(name),
      Field::NamedId(_, name) => Some(name),
    }
  }

  pub fn named(name: &'n str) -> Self {
    Self::Named(name)
  }
}

pub trait Usertype
where
  Self: UsertypeFields + UsertypeMethods + Operators + TraceableValue + Sized + 'static,
{
  const ID: Uuid;
  const VTABLE: VTable = VTable::new::<Self>();

  fn get(&self, vm: &mut Vm, this: Value, field: Field) -> UsageResult<Option<Value>> {
    match <Self as UsertypeFields>::get_field(self, vm, field.clone())? {
      Some(value) => Ok(Some(value)),
      None => <Self as UsertypeMethods>::get_method(self, vm, this, field),
    }
  }

  fn set(&mut self, vm: &mut Vm, field: Field, value: Value) -> UsageResult<()> {
    <Self as UsertypeFields>::set_field(self, vm, field, value)
  }
}

pub trait UsertypeFields {
  fn get_field(&self, vm: &mut Vm, field: Field) -> UsageResult<Option<Value>>;
  fn set_field(&mut self, vm: &mut Vm, field: Field, value: Value) -> UsageResult<()>;
}

pub trait UsertypeMethods {
  #[allow(unused_variables)]
  fn __new__(vm: &mut Vm, args: Args) -> UsageResult {
    Err(UsageError::UndefinedInitializer)
  }

  #[allow(unused_variables)]
  fn get_method(&self, vm: &mut Vm, this: Value, field: Field) -> UsageResult<Option<Value>> {
    Err(UsageError::Unimplemented("__get_method__"))
  }
}

macro_rules! unary_op {
  ($name:ident) => {
    #[allow(unused_variables)]
    fn $name(vm: &mut Vm, value: Value) -> UsageResult {
      Err(UsageError::Unimplemented(stringify!($name)))
    }
  };
}

macro_rules! binary_op {
  ($name:ident) => {
    #[allow(unused_variables)]
    fn $name(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
      Err(UsageError::Unimplemented(stringify!($name)))
    }
  };
}

macro_rules! ternary_op {
  ($name:ident) => {
    #[allow(unused_variables)]
    fn $name(vm: &mut Vm, left: Value, mid: Value, right: Value) -> UsageResult {
      Err(UsageError::Unimplemented(stringify!($name)))
    }
  };
}

pub trait Operators {
  fn __not__(_: &mut Vm, value: Value) -> UsageResult {
    Ok(!value)
  }

  unary_op!(__neg__);
  binary_op!(__add__);
  binary_op!(__sub__);
  binary_op!(__mul__);
  binary_op!(__div__);
  binary_op!(__rem__);
  binary_op!(__eq__);
  binary_op!(__neq__);
  binary_op!(__less__);
  binary_op!(__leq__);
  binary_op!(__greater__);
  binary_op!(__geq__);
  binary_op!(__index__);
  ternary_op!(__idxeq__);

  #[allow(unused_variables)]
  fn __ivk__(&mut self, vm: &mut Vm, this: Value, airity: usize) -> UsageResult {
    Err(UsageError::UndefinedMethod("__ivk__", self.__str__()))
  }

  #[allow(unused_variables)]
  fn __def__(&mut self, field: &str, value: Value) -> UsageResult<bool> {
    Err(UsageError::TypeError(self.__str__(), String::from("module")))
  }

  #[allow(unused_variables)]
  fn __res__(&self, field: &str) -> UsageResult {
    Err(UsageError::TypeError(self.__str__(), String::from("module")))
  }

  fn __str__(&self) -> String;

  fn __dbg__(&self) -> String;
}

pub trait TraceableValue {
  #[allow(unused_variables)]
  fn deep_trace(&self, marks: &mut Tracer);

  fn incremental_trace(&self, marks: &mut Tracer);
}

impl TraceableValue for Option<Value> {
  fn deep_trace(&self, marks: &mut Tracer) {
    if let Some(value) = self {
      marks.deep_trace(value);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    if let Some(value) = self {
      marks.try_mark_gray(value);
    }
  }
}

impl TraceableValue for Vec<Value> {
  fn deep_trace(&self, marks: &mut Tracer) {
    for value in self {
      marks.deep_trace(value);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    for value in self {
      marks.try_mark_gray(value);
    }
  }
}

impl<T, S> TraceableValue for HashMap<T, Value, S> {
  fn deep_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      marks.deep_trace(value);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      marks.try_mark_gray(value);
    }
  }
}

impl<T, V, S> TraceableValue for HashMap<T, V, S>
where
  V: TraceableValue,
{
  fn deep_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      value.deep_trace(marks);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      value.incremental_trace(marks);
    }
  }
}

impl<T> TraceableValue for BTreeMap<T, Value> {
  fn deep_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      marks.deep_trace(value);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      marks.try_mark_gray(value);
    }
  }
}

impl<T, V> TraceableValue for BTreeMap<T, V>
where
  V: TraceableValue,
{
  fn deep_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      value.deep_trace(marks);
    }
  }

  fn incremental_trace(&self, marks: &mut Tracer) {
    for value in self.values() {
      value.incremental_trace(marks);
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
    let mut args = args.into();
    args.push(this);
    Self { list: args }
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
    T::maybe_from(self.items.next().unwrap()).ok_or(UsageError::InvalidArgument(fn_name, pos))
  }
}

/// Intentionally empty
///
/// bool, char, i32, f64
#[derive(Default, Usertype, Fields)]
#[uuid("6d9d039a-9803-41ff-8e84-a0ea830e2380")]
pub struct Primitive {}

// all primitives can be operated on directly, no need for vtable indirection
impl Operators for Primitive {
  fn __str__(&self) -> String {
    String::from("primitive")
  }

  fn __dbg__(&self) -> String {
    String::from("primitive")
  }
}

#[methods]
impl Primitive {}

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
