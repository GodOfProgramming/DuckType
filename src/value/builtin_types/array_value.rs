use itertools::Itertools;

use super::{Usertype, UsertypeId, Value};
use crate::{ops, Env, NativeClass, NativeClassBuilder};
use std::{
  fmt::{Display, Formatter, Result},
  ops::{Deref, DerefMut},
};

#[derive(Default)]
pub struct ArrayValue {
  list: Vec<Value>,
}

impl ArrayValue {
  pub fn new_from_slice(list: &[Value]) -> Self {
    Self { list: list.into() }
  }

  pub fn new_from_vec(list: Vec<Value>) -> Self {
    Self { list }
  }
}

impl Usertype for ArrayValue {
  const ID: UsertypeId = "Array";

  fn register(class: &mut NativeClassBuilder<Self>) {
    class.constructor(|_thread, _env, args| Value::from(ArrayValue::new_from_vec(args.list)));

    class.add_method(ops::INDEX, |this, args| {
      if let Some(index) = args.first() {
        if let Ok(mut index) = index.as_i32() {
          if index < 0 {
            index = this.len() as i32 - index;
          }
          this.list.get(index as usize).cloned().unwrap_or(Value::nil)
        } else {
          Value::nil
        }
      } else {
        Value::nil
      }
    });

    class.add_method("push", |this, args| {
      this.extend(args.into_iter());
      Value::nil
    });

    class.add_method("len", |this, args| Value::from(this.list.len() as i32));
  }

  fn stringify(&self) -> String {
    format!("[{}]", self.list.iter().map(|v| v.to_string()).join(", "))
  }

  fn debug_string(&self) -> String {
    format!("{:?}", self.list)
  }
}

impl Display for ArrayValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    write!(f, "{:?}", self.list)
  }
}

impl Deref for ArrayValue {
  type Target = Vec<Value>;

  fn deref(&self) -> &Self::Target {
    &self.list
  }
}

impl DerefMut for ArrayValue {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.list
  }
}
