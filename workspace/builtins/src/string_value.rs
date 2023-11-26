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

  pub fn binop<R: Default>(&self, other: Value, f: impl FnOnce(&Self) -> R) -> R {
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
}

impl Operators for StringValue {
  #[binary]
  fn __add__(left: &StringValue, right: Value) -> UsageResult<StringValue> {
    Ok(StringValue::new(format!("{}{}", left, right)))
  }

  #[binary]
  fn __eq__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data == right.data))
  }

  #[binary]
  fn __neq__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data != right.data))
  }

  #[binary]
  fn __less__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data < right.data))
  }

  #[binary]
  fn __leq__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data <= right.data))
  }

  #[binary]
  fn __greater__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data > right.data))
  }

  #[binary]
  fn __geq__(left: &StringValue, right: Value) -> UsageResult<bool> {
    Ok(left.binop(right, |right| left.data >= right.data))
  }

  #[binary]
  fn __index__(left: &StringValue, index: i32) -> UsageResult {
    Ok(left.chars().nth(index as usize).map(|c| c.into()).unwrap_or_default())
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

impl AsRef<str> for StringValue {
  fn as_ref(&self) -> &str {
    self.as_str()
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

impl MaybeFrom<Value> for &'static String {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<StringValue>().map(|s| &**s)
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
