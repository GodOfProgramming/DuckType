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

  pub fn set(&mut self, name: impl ToString, value: Value) {}
}

impl ClassBody for InstanceValue {
  fn get_method(&self, this: &Value, name: &str) -> Option<Value> {
    if let Some(class) = self.class.as_class() {
      class.get_method(this, name)
    } else {
      None
    }
  }
}

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

  fn get_member(&self, field: &str) -> Option<Value> {
    self.data.get_member(field)
  }

  fn set_member(&mut self, field: &str, value: Value) -> ValueResult<()> {
    self.data.set(field, value);
    Ok(())
  }
}
