use super::{Error, Object, Struct, Value};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

pub struct Str {
  str: String,
  obj: Struct,

  char_at: Value,
}

impl Default for Str {
  fn default() -> Self {
    Self {
      str: Default::default(),
      obj: Default::default(),
      char_at: Value::new_native_method("char_at", |_thread, _env, this, args| {
        if this.is_str() {
          if let Some(index) = args.get(0) {
            if index.is_i32() {
              let this = this.as_str();
              this
                .chars()
                .nth(index.as_i32() as usize)
                .map(|c| c.into())
                .unwrap_or_default()
            } else {
              Value::nil
            }
          } else {
            Value::nil
          }
        } else {
          Value::nil
        }
      }),
    }
  }
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
      "char_at" => self.char_at.clone(),
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

  #[test]
  fn str_supports_char_at() {
    let s = Value::from("4321");
    let char_at = s.get("char_at");
    assert_eq!(
      char_at.method_call(
        &mut Default::default(),
        &mut Default::default(),
        s,
        vec![0.into()],
      ),
      Value::from('4')
    );
  }
}
