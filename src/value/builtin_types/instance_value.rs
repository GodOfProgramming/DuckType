use macros::class_body;

use crate::prelude::*;

#[derive(Default)]
pub struct InstanceValue {
  pub data: StructValue,
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self { data, class }
  }

  pub fn set(&mut self, name: impl ToString, value: Value) {
    self.data.set(name, value);
  }

  pub fn get(&self, name: &str) -> Value {
    self.data.get(name).or_else(|| {
      if let Some(class) = self.class.as_class() {
        class.get_method(name)
      } else {
        Value::nil
      }
    })
  }
}

#[class_body]
impl InstanceValue {}

impl Usertype for InstanceValue {
  const ID: &'static str = "Instance";

  fn stringify(&self) -> String {
    format!("<instance of {}>", self.class.stringify())
  }
}

impl Class for InstanceValue {
  fn id(&self) -> &'static str {
    "InstanceValue"
  }

  fn get(&self, field: &str) -> Option<Value> {
    Some(self.get(field))
  }

  fn set(&mut self, field: &str, value: Value) -> ValueResult<()> {
    self.set(field, value);
    Ok(())
  }
}
