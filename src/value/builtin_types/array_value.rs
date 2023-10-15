use crate::prelude::*;
use itertools::Itertools;
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default, Usertype, Fields)]
#[uuid("8b881e80-c828-4563-b7ad-d4b8f1bffbfa")]
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

#[methods]
impl ArrayValue {
  fn new(args: &Vec<Value>) -> ValueResult<Self> {
    Ok(Self::new_from_vec(args))
  }

  fn push(&mut self, value: Value) -> ValueResult<()> {
    self.list.push(value);
    Ok(())
  }

  fn len(&self) -> ValueResult<i32> {
    Ok(self.list.len() as i32)
  }

  fn __index__(&self, index: i32) -> ValueResult {
    Ok(self.list.get(index as usize).cloned().unwrap_or_default())
  }

  fn __str__(&self) -> String {
    format!("[{}]", self.list.iter().map(|v| v.to_string()).join(", "))
  }

  fn __dbg__(&self) -> String {
    format!("{:?}", self.list)
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
