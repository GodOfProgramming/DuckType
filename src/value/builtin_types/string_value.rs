use crate::prelude::*;
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default, Usertype, Fields)]
#[uuid("71d35fbb-2091-40c3-ae3c-5b62b259e8a4")]
pub struct StringValue {
  str: String,
}

#[methods]
impl StringValue {
  fn len(&self) -> ValueResult<i32> {
    Ok(self.str.len() as i32)
  }

  fn replace_with(&mut self, other: &Self) -> ValueResult<()> {
    self.str = other.str.clone();
    Ok(())
  }

  fn clone(&self) -> ValueResult<Self> {
    Ok(Self { str: self.str.clone() })
  }

  fn reverse(&self) -> ValueResult<Self> {
    Ok(Self {
      str: self.str.chars().rev().collect::<String>(),
    })
  }

  fn __add__(&self, other: Value) -> ValueResult<Self> {
    Ok(Self {
      str: format!("{}{}", self, other),
    })
  }

  fn __eq__(&self, other: &Self) -> ValueResult<bool> {
    Ok(self.str == other.str)
  }

  fn __index__(&self, index: i32) -> ValueResult {
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
    write!(f, "{}", self.str)
  }
}

impl From<String> for StringValue {
  fn from(str: String) -> Self {
    Self { str }
  }
}

impl From<&str> for StringValue {
  fn from(str: &str) -> Self {
    Self { str: str.to_string() }
  }
}

impl Deref for StringValue {
  type Target = String;

  fn deref(&self) -> &Self::Target {
    &self.str
  }
}

impl DerefMut for StringValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.str
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn default_is_empty_string() {
    assert_eq!(StringValue::default().str, String::default());
  }
}
