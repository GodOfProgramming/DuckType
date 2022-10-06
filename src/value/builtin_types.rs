use super::{AllocatedObject, VTable, Value, META_OFFSET};
pub use array::Array;
pub use class::ClassValue;
pub use closure::ClosureValue;
pub use error::ErrorValue;
pub use function::FunctionValue;
pub use instance::InstanceValue;
pub use method::MethodValue;
pub use native::{NativeClosureValue, NativeFn, NativeMethodValue};
pub use r#struct::StructValue;
use std::{
  any::TypeId,
  fmt::{Display, Formatter, Result as FmtResult},
};
pub use string::StringValue;
pub use timestamp::TimestampValue;

mod array;
mod class;
mod closure;
mod error;
mod function;
mod instance;
mod method;
mod native;
mod string;
mod r#struct;
mod timestamp;

#[derive(Debug)]
pub enum Type {
  Nil,
  F64,
  I32,
  Bool,
  Char,
  NativeFn,
  Object,

  /// Should never appear
  Undefined,
}

pub struct Nil;

pub trait ComplexValue
where
  Self: Sized + 'static,
{
  const VTABLE: VTable = VTable::new::<Self>();

  #[allow(unused_variables)]
  fn set(&mut self, name: &str, value: Value) -> Result<(), ErrorValue> {
    Err(UnimplementedFunction::Set.to_string().into())
  }

  #[allow(unused_variables)]
  fn get(&self, name: &str) -> Value {
    Value::new_err(UnimplementedFunction::Get.to_string())
  }

  #[allow(unused_variables)]
  fn add(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Add.to_string())
  }

  #[allow(unused_variables)]
  fn sub(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Sub.to_string())
  }

  #[allow(unused_variables)]
  fn mul(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Mul.to_string())
  }

  #[allow(unused_variables)]
  fn div(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Div.to_string())
  }

  #[allow(unused_variables)]
  fn rem(&self, other: Value) -> Value {
    Value::new_err(UnimplementedFunction::Rem.to_string())
  }

  #[allow(unused_variables)]
  fn index(&self, index: Value) -> Value {
    Value::new_err(UnimplementedFunction::Index.to_string())
  }

  fn drop(&mut self) {}

  // override this only if necessary

  fn dealloc(this: *mut Self) {
    consume::<Self>(this);
  }

  // below this line should not to be reimplemented by the user

  fn type_id() -> TypeId {
    TypeId::of::<Self>()
  }

  fn basic_desc() -> &'static str {
    std::any::type_name::<Self>()
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

impl From<Vec<Value>> for Args {
  fn from(list: Vec<Value>) -> Self {
    Self { this: None, list }
  }
}

impl From<(Value, Vec<Value>)> for Args {
  fn from((this, list): (Value, Vec<Value>)) -> Self {
    Self {
      this: Some(this),
      list,
    }
  }
}

impl<T: Into<Value> + Clone, const I: usize> From<(Value, [T; I])> for Args {
  fn from((this, list): (Value, [T; I])) -> Self {
    Self {
      this: Some(this),
      list: list
        .into_iter()
        .cloned()
        .map(|v| -> Value { v.into() })
        .collect(),
    }
  }
}

pub enum UnimplementedFunction {
  Set,
  Get,
  Call,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Index,
  Custom(String),
}

impl Display for UnimplementedFunction {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{} is unimplemented",
      match self {
        UnimplementedFunction::Set => "set",
        UnimplementedFunction::Get => "get",
        UnimplementedFunction::Call => "call",
        UnimplementedFunction::Add => "add",
        UnimplementedFunction::Sub => "sub",
        UnimplementedFunction::Mul => "mul",
        UnimplementedFunction::Div => "div",
        UnimplementedFunction::Rem => "rem",
        UnimplementedFunction::Index => "index",
        UnimplementedFunction::Custom(s) => s,
      }
    )
  }
}

fn consume<T: ComplexValue>(this: *mut T) -> Box<AllocatedObject<T>> {
  unsafe { Box::from_raw((this as *mut u8).offset(META_OFFSET) as *mut AllocatedObject<T>) }
}
