use super::{AllocatedObject, UsertypeId, VTable, Value, META_OFFSET};
use crate::Env;
pub use array_value::ArrayValue;
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use error_value::ErrorValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
pub use method_value::MethodValue;
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
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

pub struct Nil;

pub trait Usertype
where
  Self: Sized,
{
  const ID: UsertypeId;
  const VTABLE: VTable = VTable::new::<Self>();

  fn register(class: &mut NativeClass) {}

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

use std::collections::BTreeMap;
type ClassGetter = fn(Value, &str) -> Value;
type ClassSetter = fn(Value, &str, Value);

pub struct NativeClass {
  name: &'static str,
  constructor: Option<NativeFn>,
  methods: BTreeMap<String, NativeFn>,
  any_getter: Option<ClassGetter>,
  any_setter: Option<ClassSetter>,
  property_getters: BTreeMap<String, ClassGetter>,
  property_setters: BTreeMap<String, ClassSetter>,
}

impl NativeClass {
  pub fn new<T: Usertype>() -> Self {
    Self {
      name: T::ID,
      constructor: None,
      methods: BTreeMap::new(),
      any_getter: None,
      any_setter: None,
      property_getters: BTreeMap::new(),
      property_setters: BTreeMap::new(),
    }
  }

  pub fn name(&self) -> &'static str {
    self.name
  }

  pub fn constructor(&mut self, f: NativeFn) {
    self.constructor = Some(f);
  }

  pub fn method(&mut self, name: impl ToString, f: NativeFn) {
    self.methods.insert(name.to_string(), f);
  }

  pub fn define_any_getter(&mut self, f: ClassGetter) {
    self.any_getter = Some(f);
  }

  pub fn define_any_setter(&mut self, f: ClassSetter) {
    self.any_setter = Some(f);
  }

  pub fn define_property_getter(&mut self, name: impl ToString, getter: ClassGetter) {
    self.property_getters.insert(name.to_string(), getter);
  }

  pub fn define_property_setter(&mut self, name: impl ToString, setter: ClassSetter) {
    self.property_setters.insert(name.to_string(), setter);
  }
}

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

  fn register(class: &mut NativeClass) {}
}
