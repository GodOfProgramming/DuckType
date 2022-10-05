use crate::Args;

use super::{ComplexValue, Value};
use std::{
  fmt::{Display, Formatter, Result},
  ops::{Deref, DerefMut, Index},
};

pub struct ArrayValue;

impl ArrayValue {
  fn len(_thread: i32, _env: i32, args: Args) -> Value {
    let mut args = args.list.into_iter();
    if let Some(arr) = args.next() {
      if let Ok(arr) = arr.as_array() {
        Value::from(arr.len() as i32)
      } else {
        Value::new_err("cannot compute length of non array")
      }
    } else {
      Value::new_err("cannot compute length of nothing")
    }
  }
}

#[derive(Default)]
pub struct Array(Vec<Value>);

impl From<&[Value]> for Array {
  fn from(vec: &[Value]) -> Self {
    Self(vec.into())
  }
}

impl From<Vec<Value>> for Array {
  fn from(vec: Vec<Value>) -> Self {
    Self(vec)
  }
}

impl ComplexValue for Array {}

impl Display for Array {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{:?}", self.0)
  }
}

impl Deref for Array {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Array {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Index<usize> for Array {
  type Output = Value;

  fn index(&self, index: usize) -> &Self::Output {
    &self.0[index]
  }
}
