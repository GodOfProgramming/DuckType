use crate::{ops, NativeClassBuilder};

use super::{Usertype, UsertypeId, Value};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct StringValue {
  str: String,
}

impl StringValue {
  fn char_at(&self, index: &Value) -> Value {
    if let Ok(indx) = index.as_i32() {
      self.chars().nth(indx as usize).map(|c| c.into()).unwrap_or_default()
    } else {
      Value::nil
    }
  }
}

impl Usertype for StringValue {
  const ID: UsertypeId = "String";

  fn register(class: &mut NativeClassBuilder<Self>) {
    class.add_getter("len", |this| Value::from(this.len() as i32));

    class.add_method(ops::INDEX, |this, args| this.char_at(args.first().unwrap_or(&Value::nil)));

    class.add_method(ops::ADD, |this, args| {
      if let Some(other) = args.first() {
        Value::from(format!("{}{}", this.deref(), other))
      } else {
        Value::new_err("somehow called add without argument")
      }
    });

    class.add_method(ops::EQUALITY, |this, args| {
      if let Some(other) = args.first() {
        if let Ok(other) = other.as_str() {
          return Value::from(**this == **other);
        }
      }

      Value::from(false)
    });
  }

  fn stringify(&self) -> String {
    self.deref().clone()
  }

  fn debug_string(&self) -> String {
    format!("\"{}\"", self.stringify())
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
