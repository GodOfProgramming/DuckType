use crate::prelude::*;
use std::{
  collections::hash_map::DefaultHasher,
  fmt::{Display, Formatter, Result as FmtResult},
  hash::Hash,
  ops::{Deref, DerefMut},
};

#[derive(Default, Usertype, Fields)]
#[uuid("71d35fbb-2091-40c3-ae3c-5b62b259e8a4")]
pub struct StringValue {
  data: String,
  hash: DefaultHasher,
}

impl StringValue {
  pub fn new(data: impl ToString) -> Self {
    let data = data.to_string();
    let mut hash = DefaultHasher::new();
    data.hash(&mut hash);
    Self { data, hash }
  }

  pub fn do_with_other<R: Default>(&self, other: Value, f: impl FnOnce(&Self) -> R) -> R {
    other.cast_to::<Self>().map(f).unwrap_or_default()
  }
}

#[methods]
impl StringValue {
  fn len(&self) -> UsageResult<i32> {
    Ok(self.data.len() as i32)
  }

  fn replace_with(&mut self, other: &Self) -> UsageResult<()> {
    self.data = other.data.clone();
    Ok(())
  }

  fn clone(&self) -> UsageResult<Self> {
    Ok(Self {
      data: self.data.clone(),
      hash: self.hash.clone(),
    })
  }

  fn reverse(&self) -> UsageResult<Self> {
    Ok(Self::new(self.data.chars().rev().collect::<String>()))
  }

  fn __add__(&self, other: Value) -> UsageResult<Self> {
    Ok(Self::new(format!("{}{}", self, other)))
  }

  fn __eq__(&self, other: Value) -> UsageResult<bool> {
    Ok(self.do_with_other(other, |other| self.data == other.data))
  }

  fn __neq__(&self, other: Value) -> UsageResult<bool> {
    self.__eq__(other).map(|v| !v)
  }

  fn __less__(&self, other: Value) -> UsageResult<bool> {
    Ok(self.do_with_other(other, |other| self.data < other.data))
  }

  fn __leq__(&self, other: Value) -> UsageResult<bool> {
    Ok(self.do_with_other(other, |other| self.data <= other.data))
  }

  fn __greater__(&self, other: Value) -> UsageResult<bool> {
    Ok(self.do_with_other(other, |other| self.data > other.data))
  }

  fn __geq__(&self, other: Value) -> UsageResult<bool> {
    Ok(self.do_with_other(other, |other| self.data >= other.data))
  }

  fn __index__(&self, index: i32) -> UsageResult {
    Ok(self.chars().nth(index as usize).map(|c| c.into()).unwrap_or_default())
  }

  fn __str__(&self) -> String {
    self.deref().clone()
  }

  fn __dbg__(&self) -> String {
    format!("\"{}\"", self.__str__())
  }
}

impl Display for StringValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.data)
  }
}

impl From<String> for StringValue {
  fn from(str: String) -> Self {
    Self::new(str)
  }
}

impl From<&str> for StringValue {
  fn from(str: &str) -> Self {
    Self::new(str)
  }
}

impl Deref for StringValue {
  type Target = String;

  fn deref(&self) -> &Self::Target {
    &self.data
  }
}

impl DerefMut for StringValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.data
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn default_is_empty_string() {
    assert_eq!(StringValue::default().data, String::default());
  }
}
