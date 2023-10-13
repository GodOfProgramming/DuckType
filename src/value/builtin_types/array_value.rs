use crate::prelude::*;
use itertools::Itertools;
use macros::{class_body, Class};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Class, Default)]
pub struct ArrayValue {
  list: Vec<Value>,
}

impl ArrayValue {
  pub fn new_from_slice(list: &[Value]) -> Self {
    Self { list: list.into() }
  }

  pub fn new_from_vec(list: &Vec<Value>) -> Self {
    Self { list: list.clone() }
  }
}

#[class_body]
impl ArrayValue {
  fn new(args: &Vec<Value>) -> Self {
    Self::new_from_vec(args)
  }

  fn __index__(&self, index: i32) -> Value {
    self.list.get(index as usize).cloned().unwrap_or_default()
  }

  fn push(&mut self, value: Value) {
    self.list.push(value);
  }

  fn len(&self) -> i32 {
    self.list.len() as i32
  }
}

impl Usertype for ArrayValue {
  const ID: &'static str = "Array";

  fn stringify(&self) -> String {
    format!("[{}]", self.list.iter().map(|v| v.to_string()).join(", "))
  }

  fn debug_string(&self) -> String {
    format!("{:?}", self.list)
  }
}

impl MaybeFrom<&Value> for &ArrayValue {
  fn maybe_from(value: &Value) -> Option<Self> {
    value.cast_to()
  }
}

impl Display for ArrayValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{:?}", self.list)
  }
}

impl Deref for ArrayValue {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.list
  }
}

impl DerefMut for ArrayValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.list
  }
}
