use crate::prelude::*;

#[derive(Usertype, Default)]
#[uuid("988a6bd1-4a54-416f-aad5-0d1cc8ce652e")]
pub struct InstanceValue {
  pub data: StructValue,
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self { data, class }
  }
}

impl UsertypeFields for InstanceValue {
  fn get_field(&self, field: &str) -> ValueResult<Option<Value>> {
    if field == "__class__" {
      Ok(Some(self.class.clone()))
    } else {
      self.data.get_field(field)
    }
  }

  fn set_field(&mut self, field: &str, value: Value) -> ValueResult<()> {
    if field == "__class__" {
      Err(ValueError::Immutable(field.to_string()))
    } else {
      Ok(self.data.set(field, value))
    }
  }
}

impl UsertypeMethods for InstanceValue {
  fn get_method(&self, this: &Value, name: &str) -> ValueResult<Option<Value>> {
    if let Some(class) = self.class.as_class() {
      Ok(class.get_method(this, name))
    } else {
      Ok(None)
    }
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
