use crate::{Args, Env, ExecutionThread};

use super::{Class, ComplexValue, ErrorValue, StructValue, Value};
use std::{
  fmt::{Display, Formatter, Result as FmtResult},
  ops::{Deref, DerefMut},
};

pub struct StringClass;

impl StringClass {
  fn char_at(_thread: &mut ExecutionThread, _env: &mut Env, args: Args) -> Value {
    if let Some(this) = args.this {
      if let Ok(this) = this.as_str() {
        if let Some(index) = args.list.first() {
          if let Ok(indx) = index.as_i32() {
            this.chars().nth(indx as usize).map(|c| c.into()).unwrap_or_default()
          } else {
            Value::nil
          }
        } else {
          Value::nil
        }
      } else {
        Value::new_err("cannot index non-string type")
      }
    } else {
      Value::new_err("unable to call char_at without this parameter")
    }
  }
}

impl Class for StringClass {
  fn call(method: &str) -> Value {
    match method {
      "char_at" => Value::new_native_fn(Self::char_at),
      _ => Value::nil,
    }
  }
}

pub struct StringValue {
  str: String,
  obj: StructValue,
}

impl Default for StringValue {
  fn default() -> Self {
    Self {
      str: Default::default(),
      obj: Default::default(),
    }
  }
}

impl ComplexValue for StringValue {
  fn set(&mut self, name: &str, value: Value) -> Result<(), ErrorValue> {
    self.obj.set(name, value)
  }

  fn get(&self, name: &str) -> Value {
    println!("{}", name);
    self.obj.get(name).or_else(|| match name {
      "len" => {
        let result = self.len() as i32;
        if result as usize != self.len() {
          Value::new_err(format!("string too big to calculate length, limit is {}", i32::MAX))
        } else {
          result.into()
        }
      }
      method => StringClass::call(method),
    })
  }

  fn add(&self, other: Value) -> Value {
    Value::from(format!("{}{}", self.deref(), other))
  }

  fn eq(&self, other: &Value) -> bool {
    if let Ok(other) = other.cast_to::<Self>() {
      self.deref().eq(other.deref())
    } else {
      false
    }
  }

  fn cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
    if let Ok(other) = other.cast_to::<Self>() {
      self.deref().partial_cmp(other.deref())
    } else {
      None
    }
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
  use crate::Args;

  use super::*;

  #[test]
  fn default_is_empty_string() {
    assert_eq!(StringValue::default().str, String::default());
  }

  #[test]
  fn str_len_gets_correct_value() {
    let s = StringValue::from("0123456789");
    assert_eq!(s.get("len"), 10.into());
  }

  #[test]
  fn str_supports_overriding_fields() {
    let mut s = StringValue::from("0123456789");
    s.set("len", Value::from(0)).unwrap();
    assert_eq!(s.get("len"), 0.into());
  }

  #[test]
  fn str_supports_char_at() {
    let s = Value::from("4321");
    let char_at = s.get("char_at").as_native_fn().unwrap();
    let args = Args::from((s, [0]));

    assert_eq!(
      char_at(&mut Default::default(), &mut Default::default(), args,),
      Value::from('4')
    );
  }
}
