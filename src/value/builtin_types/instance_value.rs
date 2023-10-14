use crate::prelude::*;

#[derive(Usertype, Default)]
pub struct InstanceValue {
  pub data: StructValue,
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self { data, class }
  }
}

impl ClassMethods for InstanceValue {
  fn get_method(&self, this: &Value, name: &str) -> Option<Value> {
    if let Some(class) = self.class.as_class() {
      class.get_method(this, name)
    } else {
      None
    }
  }
}

impl ClassFields for InstanceValue {
  fn get_member(&self, field: &str) -> Option<Value> {
    self.data.get_member(field)
  }

  fn set_member(&mut self, field: &str, value: Value) -> ValueResult<()> {
    self.data.set(field, value);
    Ok(())
  }
}

impl DisplayValue for InstanceValue {
  fn __str__(&self) -> String {
    format!("<instance of {}>", self.class)
  }
}

impl DebugValue for InstanceValue {
  fn __dbg__(&self) -> String {
    self.__str__()
  }
}

impl LockableValue for InstanceValue {}
