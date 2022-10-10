use super::{ComplexValue, StructValue, Value};

#[derive(Default)]
pub struct InstanceValue {
  pub data: StructValue,
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self {
      data,
      class,
      ..Default::default()
    }
  }
}

impl ComplexValue for InstanceValue {
  fn set(&mut self, name: &str, value: Value) -> Result<(), crate::ErrorValue> {
    self.data.set(name, value).ok();
    Ok(())
  }

  fn get(&self, name: &str) -> Value {
    self.data.get(&name.to_string()).or_else(|| {
      if let Ok(class) = self.class.as_class() {
        class.get_method(&name.to_string())
      } else {
        Value::nil
      }
    })
  }

  fn stringify(&self) -> String {
    format!("<instance of {}>", self.class.stringify())
  }
}
