use crate::prelude::*;

#[derive(Usertype, Default)]
#[uuid("988a6bd1-4a54-416f-aad5-0d1cc8ce652e")]
pub struct InstanceValue {
  #[trace]
  pub data: StructValue,
  #[trace]
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self { data, class }
  }
}

impl UsertypeFields for InstanceValue {
  fn get_field(&self, gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
    if field == "__class__" {
      Ok(Some(self.class.clone()))
    } else {
      self.data.get_field(gc, field)
    }
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
    if field == "__class__" {
      Err(ValueError::Immutable(field.to_string()))
    } else {
      self.data.set(field, value);
      Ok(())
    }
  }
}

impl UsertypeMethods for InstanceValue {
  fn get_method(&self, gc: &mut Gc, this: &Value, name: &str) -> ValueResult<Option<Value>> {
    if let Some(class) = self.class.as_class() {
      Ok(class.get_method(gc, this, name))
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
