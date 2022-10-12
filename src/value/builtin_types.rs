use super::{AllocatedObject, ComplexValueId, VTable, Value, META_OFFSET};
pub use array_value::ArrayValue;
pub use class_value::ClassValue;
pub use closure_value::ClosureValue;
pub use error_value::ErrorValue;
pub use function_value::FunctionValue;
pub use instance_value::InstanceValue;
pub use method_value::MethodValue;
pub use native_value::{NativeClosureValue, NativeFn, NativeMethodValue};
use std::cmp::Ordering;
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

pub trait ComplexValue
where
  Self: Sized,
{
  const ID: ComplexValueId;
  const VTABLE: VTable = VTable::new::<Self>();

  #[allow(unused_variables)]
  fn set(&mut self, name: &str, value: Value) -> Value {
    Value::new_err(UnimplementedFunction::Set.fmt(self))
  }

  #[allow(unused_variables)]
  fn get(&self, name: &str) -> Value {
    Value::new_err(UnimplementedFunction::Get.fmt(self))
  }

  #[allow(unused_variables)]
  fn index(&self, index: Value) -> Value {
    Value::new_err(UnimplementedFunction::Index.fmt(self))
  }

  #[allow(unused_variables)]
  fn add(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Add.fmt(self))
  }

  #[allow(unused_variables)]
  fn sub(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Sub.fmt(self))
  }

  #[allow(unused_variables)]
  fn mul(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Mul.fmt(self))
  }

  #[allow(unused_variables)]
  fn div(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Div.fmt(self))
  }

  #[allow(unused_variables)]
  fn rem(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Rem.fmt(self))
  }

  #[allow(unused_variables)]
  fn neg(&self) -> Value {
    Value::new_err(UnimplementedFunction::Neg.fmt(self))
  }

  #[allow(unused_variables)]
  fn not(&self) -> Value {
    Value::new_err(UnimplementedFunction::Not.fmt(self))
  }

  #[allow(unused_variables)]
  fn eq(&self, other: &Value) -> bool {
    false
  }

  #[allow(unused_variables)]
  fn cmp(&self, other: &Value) -> Option<Ordering> {
    None
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

  fn type_id() -> ComplexValueId {
    Self::ID
  }

  fn type_name() -> String {
    std::any::type_name::<Self>().to_string()
  }
}

pub trait Class {
  fn call(method: &str) -> Value;
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
  fn fmt<T: ComplexValue>(&self, v: &T) -> String {
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

fn consume<T: ComplexValue>(this: *mut T) -> Box<AllocatedObject<T>> {
  unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) }
}

/// Intentionally empty
pub struct Primitive;

impl ComplexValue for Primitive {
  const ID: ComplexValueId = "";
}
