use super::{ComplexValue, ComplexValueId, Value};
use std::{
  fmt::{Display, Formatter, Result},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct ArrayValue(Vec<Value>);

impl From<&[Value]> for ArrayValue {
  fn from(vec: &[Value]) -> Self {
    Self(vec.into())
  }
}

impl From<Vec<Value>> for ArrayValue {
  fn from(vec: Vec<Value>) -> Self {
    Self(vec)
  }
}

impl ComplexValue for ArrayValue {
  const ID: ComplexValueId = "Array";

  fn index(&self, index: Value) -> Value {
    if let Ok(mut value) = index.as_i32() {
      if value < 0 {
        value = self.len() as i32 - value;
      }
      self.0.get(value as usize).cloned().unwrap_or(Value::nil)
    } else {
      Value::nil
    }
  }
}

impl Display for ArrayValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{:?}", self.0)
  }
}

impl Deref for ArrayValue {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for ArrayValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}
