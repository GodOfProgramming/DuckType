use crate::prelude::*;
use macros::{class_body, Class};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default, Class)]
pub struct StringValue {
  str: String,
}

#[class_body]
impl StringValue {
  fn len(&self) -> i32 {
    self.str.len() as i32
  }

  fn clone(&self) -> Self {
    Self { str: self.str.clone() }
  }

  fn reverse(&self) -> Self {
    Self {
      str: self.str.chars().rev().collect::<String>(),
    }
  }

  fn __add__(&self, other: &Self) -> Self {
    Self {
      str: format!("{}{}", self, other),
    }
  }

  fn __eq__(&self, other: &Self) -> bool {
    self.str == other.str
  }

  fn index(&self, index: i32) -> Value {
    self.chars().nth(index as usize).map(|c| c.into()).unwrap_or_default()
  }

  fn eq(&self, other: &Self) -> bool {
    self.str == other.str
  }
}

impl Usertype for StringValue {
  const ID: &'static str = "String";

  fn stringify(&self) -> String {
    self.deref().clone()
  }

  fn debug_string(&self) -> String {
    format!("\"{}\"", self.stringify())
  }
}

impl MaybeFrom<Value> for &'static StringValue {
  fn maybe_from(value: Value) -> Option<Self> {
    value.cast_to::<StringValue>()
  }
}

impl Display for StringValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.str)
  }
}

impl From<String> for StringValue {
  fn from(str: String) -> Self {
    Self {
      str,
      ..Default::default()
    }
  }
}

impl From<&str> for StringValue {
  fn from(str: &str) -> Self {
    Self {
      str: str.to_string(),
      ..Default::default()
    }
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
