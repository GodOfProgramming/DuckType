use crate::prelude::*;
use rand::seq::SliceRandom;
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default, Usertype, Fields)]
#[uuid("8b881e80-c828-4563-b7ad-d4b8f1bffbfa")]
pub struct VecValue {
  #[trace]
  pub(crate) buffer: Vec<Value>,
}

impl VecValue {
  pub fn new_from_slice(list: &[Value]) -> Self {
    Self { buffer: list.into() }
  }

  pub fn new_from_vec(list: Vec<Value>) -> Self {
    Self { buffer: list }
  }
}

#[methods]
impl VecValue {
  fn __new__(args: &[Value]) -> ValueResult<VecValue> {
    Ok(VecValue::new_from_vec(args.to_owned()))
  }

  fn push(&mut self, value: Value) -> ValueResult<()> {
    self.buffer.push(value);
    Ok(())
  }

  fn len(&self) -> ValueResult<i32> {
    Ok(self.buffer.len() as i32)
  }

  fn insert(&mut self, index: i32, value: Value) -> ValueResult<Value> {
    if let Some(v) = self.buffer.get_mut(index as usize) {
      *v = value.clone();
      Ok(value)
    } else {
      Err(ValueError::InvalidIndex(index, value))
    }
  }

  fn random_index(&self) -> ValueResult {
    Ok(self.buffer.choose(&mut rand::thread_rng()).cloned().unwrap_or_default())
  }

  fn join(&self, sep: Value) -> ValueResult<String> {
    if sep.is_nil() {
      Ok(itertools::join(self.buffer.iter(), ""))
    } else {
      Ok(itertools::join(self.buffer.iter(), &sep.to_string()))
    }
  }

  fn __index__(&self, index: i32) -> ValueResult {
    Ok(self.buffer.get(index as usize).cloned().unwrap_or_default())
  }

  fn __idxeq__(&mut self, index: i32, value: Value) -> ValueResult {
    let internal_value = self
      .buffer
      .get_mut(index as usize)
      .ok_or_else(|| ValueError::InvalidIndex(index, value.clone()))?;
    *internal_value = value.clone();
    Ok(value)
  }

  fn __str__(&self) -> String {
    format!("[{}]", itertools::join(self.buffer.iter(), ", "))
  }

  fn __dbg__(&self) -> String {
    format!("{:?}", self.buffer)
  }
}

impl Display for VecValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{:?}", self.buffer)
  }
}

impl Deref for VecValue {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.buffer
  }
}

impl DerefMut for VecValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.buffer
  }
}
