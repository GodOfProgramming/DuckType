use super::{AllocatedObject, VTable, Value, META_OFFSET};
use crate::prelude::*;
pub use array_value::ArrayValue;
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use error_value::ErrorValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
pub use method_value::MethodValue;
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::collections::BTreeMap;
pub use string_value::StringValue;
pub use struct_value::StructValue;
pub use timestamp_value::TimestampValue;

mod array_value;
mod class_value;
mod closure_value;
mod error_value;
mod function_value;
mod instance_value;
mod method_value;
mod native_value;
mod string_value;
mod struct_value;
mod timestamp_value;

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

pub struct Nil;

pub type UsertypeId = &'static str;
pub trait Usertype: 'static
where
  Self: Sized,
{
  const ID: UsertypeId;
  const VTABLE: VTable = VTable::new::<Self>();

  #[allow(unused)]
  fn register(class: &mut NativeClassBuilder<Self>) {}

  fn class(env: &Env) -> Value {
    env.lookup(Self::ID).unwrap_or_default()
  }

  fn stringify(&self) -> String {
    Self::type_name()
  }

  fn debug_string(&self) -> String {
    self.stringify()
  }

  fn drop(&mut self) {}

  // override this only if necessary

  fn dealloc(this: *mut Self) {
    consume::<Self>(this);
  }

  // below this line should not to be reimplemented by the user

  fn type_id() -> UsertypeId {
    Self::ID
  }

  fn type_name() -> String {
    std::any::type_name::<Self>().to_string()
  }
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

  pub(crate) fn construct(&self, this_class: Value, vm: &mut Vm, env: &mut Env, mut args: Args) -> Value {
    let this = Value::from(InstanceValue::new(Default::default(), this_class));
    if let Some(constructor) = self.constructor {
      args.this = Some(this);
      constructor(vm, env, args)
    } else {
      this
    }
  }

  pub(crate) fn get_method(&self, name: &str) -> Value {
    self.methods.get(name).cloned().unwrap_or_default()
  }
}

pub struct NativeClassBuilder<T: Usertype> {
  constructor: Option<NativeFn>,
  methods: BTreeMap<String, fn(&mut T, Vec<Value>) -> Value>,
  statics: BTreeMap<String, fn(Vec<Value>) -> Value>,
  setters: BTreeMap<String, fn(&mut T, Value)>,
  getters: BTreeMap<String, fn(&T) -> Value>,
}

impl<T: Usertype> NativeClassBuilder<T> {
  pub fn new() -> Self {
    Self {
      constructor: None,
      methods: Default::default(),
      statics: Default::default(),
      setters: Default::default(),
      getters: Default::default(),
    }
  }

  pub fn constructor(&mut self, f: NativeFn) {
    self.constructor = Some(f);
  }

  pub fn add_method(&mut self, name: impl ToString, f: fn(&mut T, Vec<Value>) -> Value) {
    self.methods.insert(name.to_string(), f);
  }

  pub fn add_static(&mut self, name: impl ToString, f: fn(Vec<Value>) -> Value) {
    self.statics.insert(name.to_string(), f);
  }

  pub fn add_setter(&mut self, name: impl ToString, f: fn(&mut T, Value)) {
    self.setters.insert(name.to_string(), f);
  }

  pub fn add_getter(&mut self, name: impl ToString, f: fn(&T) -> Value) {
    self.getters.insert(name.to_string(), f);
  }

  pub fn build(self) -> NativeClass {
    NativeClass {
      name: T::ID,
      constructor: self.constructor,
      methods: self
        .methods
        .into_iter()
        .map(|(name, m)| {
          (
            name.clone(),
            NativeMethodValue::new_native_closure(NativeClosureValue::new(name, move |_vm, _env, args| {
              if let Some(mut this) = args.this {
                if let Ok(this) = this.cast_to_mut::<T>() {
                  return m(this, args.list);
                }
              }
              Value::nil
            }))
            .into(),
          )
        })
        .collect(),
      statics: self
        .statics
        .into_iter()
        .map(|(name, s)| {
          (
            name.clone(),
            Value::new_native_closure(name, move |_vm, _env, args| s(args.list)),
          )
        })
        .collect(),
      setters: self
        .setters
        .into_iter()
        .map(|(name, setter)| {
          (
            name,
            SetterConv::new(move |this, value| {
              if let Ok(this) = this.cast_to_mut::<T>() {
                setter(this, value)
              }
            })
            .into(),
          )
        })
        .collect(),
      getters: self
        .getters
        .into_iter()
        .map(|(name, getter)| {
          (
            name,
            GetterConv::new(move |this| {
              if let Ok(this) = this.cast_to::<T>() {
                getter(this)
              } else {
                Value::nil
              }
            })
            .into(),
          )
        })
        .collect(),
    }
  }
}

struct SetterConv(Box<dyn Fn(&mut Value, Value)>);

impl SetterConv {
  fn new<F>(f: F) -> Self
  where
    F: Fn(&mut Value, Value) + 'static,
  {
    Self(Box::new(f))
  }
}

impl Into<Box<dyn Fn(&mut Value, Value)>> for SetterConv {
  fn into(self) -> Box<dyn Fn(&mut Value, Value)> {
    self.0
  }
}

struct GetterConv(Box<dyn Fn(&Value) -> Value>);

impl GetterConv {
  fn new<F>(f: F) -> Self
  where
    F: Fn(&Value) -> Value + 'static,
  {
    Self(Box::new(f))
  }
}

impl Into<Box<dyn Fn(&Value) -> Value>> for GetterConv {
  fn into(self) -> Box<dyn Fn(&Value) -> Value> {
    self.0
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

fn consume<T: Usertype>(this: *mut T) -> Box<AllocatedObject<T>> {
  unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) }
}

/// Intentionally empty
pub struct Primitive;

impl Usertype for Primitive {
  const ID: UsertypeId = "";
}
