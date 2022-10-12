use crate::{Env, NativeClass};

use super::{Usertype, UsertypeId, Value};
use std::{
  fmt::{Display, Formatter, Result},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct ArrayValue(Vec<Value>);

impl From<&[Value]> for ArrayValue {
  fn from(vec: &[Value]) -> Self {
    Self(vec.into())
  }
}

impl From<Vec<Value>> for ArrayValue {
  fn from(vec: Vec<Value>) -> Self {
    Self(vec)
  }
}

impl Usertype for ArrayValue {
  const ID: UsertypeId = "Array";

  fn register(class: &mut NativeClass) {
    class.constructor(|_thread, _env, args| Value::from(ArrayValue::from(args.list)));

    class.method("__index__", |_thread, _env, args| {
      if let Some(this) = args.this {
        if let Ok(this) = this.as_array() {
          if let Some(index) = args.list.first() {
            if let Ok(mut index) = index.as_i32() {
              if index < 0 {
                index = this.len() as i32 - index;
              }
              this.0.get(index as usize).cloned().unwrap_or(Value::nil)
            } else {
              Value::nil
            }
          } else {
            Value::nil
          }
        } else {
          Value::nil
        }
      } else {
        Value::nil
      }
    })
  }
}

impl Display for ArrayValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{:?}", self.0)
  }
}

impl Deref for ArrayValue {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for ArrayValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}
