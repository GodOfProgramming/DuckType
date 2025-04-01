use crate::prelude::*;
use rand::seq::IndexedRandom;
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

#[methods]
impl VecValue {
  fn __new__(args: &[Value]) -> UsageResult<VecValue> {
    Ok(VecValue::from(args))
  }

  fn push(&mut self, value: Value) -> UsageResult<()> {
    self.buffer.push(value);
    Ok(())
  }

  fn len(&self) -> UsageResult<i32> {
    Ok(self.buffer.len() as i32)
  }

  fn insert(&mut self, index: i32, value: Value) -> UsageResult<Value> {
    if let Some(v) = self.buffer.get_mut(index as usize) {
      *v = value;
      Ok(value)
    } else {
      Err(UsageError::InvalidIndex(index, value))
    }
  }

  fn random_index(&self) -> UsageResult {
    Ok(self.buffer.choose(&mut rand::rng()).cloned().unwrap_or_default())
  }

  fn join(&self, sep: Value) -> UsageResult<String> {
    if sep.is::<()>() {
      Ok(itertools::join(self.buffer.iter(), ""))
    } else {
      Ok(itertools::join(self.buffer.iter(), &sep.to_string()))
    }
  }
}

impl Operators for VecValue {
  #[binary]
  fn __index__(left: &VecValue, index: i32) -> UsageResult {
    Ok(left.buffer.get(index as usize).cloned().unwrap_or_default())
  }

  #[ternary]
  fn __idxeq__(left: &mut VecValue, index: i32, value: Value) -> UsageResult {
    let internal_value = left
      .buffer
      .get_mut(index as usize)
      .ok_or(UsageError::InvalidIndex(index, value))?;
    *internal_value = value;
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

impl From<Vec<Value>> for VecValue {
  fn from(value: Vec<Value>) -> Self {
    Self { buffer: value }
  }
}

impl<const I: usize> From<[Value; I]> for VecValue {
  fn from(value: [Value; I]) -> Self {
    Self { buffer: value.into() }
  }
}

impl From<&[Value]> for VecValue {
  fn from(value: &[Value]) -> Self {
    Self { buffer: value.into() }
  }
}

impl<T> From<&T> for VecValue
where
  Self: From<T>,
  T: Clone,
{
  fn from(value: &T) -> Self {
    Self::from(value.clone())
  }
}
