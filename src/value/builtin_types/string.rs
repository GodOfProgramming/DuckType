use super::{Error, Object, Struct, Value};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct Str {
  str: String,
  obj: Struct,
}

impl Object for Str {
  fn set(&mut self, name: &str, value: Value) -> Result<(), Error> {
    self.obj.set(name, value)
  }

  fn get(&self, name: &str) -> Value {
    self.obj.get(name).or_else(|| match name {
      "len" => {
        let result = self.len() as i32;
        if result as usize != self.len() {
          Value::new_err(format!(
            "string too big to calculate length, limit is {}",
            i32::MAX
          ))
        } else {
          result.into()
        }
      }
      _ => Value::nil,
    })
  }
}

impl Display for Str {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.str)
  }
}

impl From<String> for Str {
  fn from(str: String) -> Self {
    Self {
      str,
      ..Default::default()
    }
  }
}

impl From<&str> for Str {
  fn from(str: &str) -> Self {
    Self {
      str: str.to_string(),
      ..Default::default()
    }
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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn default_is_empty_string() {
    assert_eq!(Str::default().str, String::default());
  }

  #[test]
  fn str_len_gets_correct_value() {
    let s = Str::from("0123456789");
    assert_eq!(s.get("len"), 10.into());
  }

  #[test]
  fn str_supports_overriding_fields() {
    let mut s = Str::from("0123456789");
    s.set("len", Value::from(0)).unwrap();
    assert_eq!(s.get("len"), 0.into());
  }
}
