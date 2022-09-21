use super::{Object, Value};
use std::fmt::{Display, Formatter, Result};

#[derive(Default)]
pub struct Array {
  vec: Vec<Value>,
}

impl From<&[Value]> for Array {
  fn from(vec: &[Value]) -> Self {
    Self { vec: vec.into() }
  }
}

impl Object for Array {}

impl Display for Array {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{:?}", self.vec)
  }
}
