use super::{Object, Value};
use std::{
  fmt::{Display, Formatter, Result},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct Str {
  str: String,
}

impl Object for Str {
  fn get(&self, name: &str) -> Value {
    match name {
      "len" => {
        let result = self.len() as i32;
        if result as usize != self.len() {
          Value::new_err("")
        } else {
          result.into()
        }
      }
      _ => Value::new_err(format!("no method '{}' for string", name)),
    }
  }
}

impl Display for Str {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", self.str)
  }
}

impl From<String> for Str {
  fn from(str: String) -> Self {
    Self { str }
  }
}

impl Deref for Str {
  type Target = String;

  fn deref(&self) -> &Self::Target {
    &self.str
  }
}

impl DerefMut for Str {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.str
  }
}
