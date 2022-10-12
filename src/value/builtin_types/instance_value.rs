use super::{StructValue, Usertype, UsertypeId, Value};

#[derive(Default)]
pub struct InstanceValue {
  pub data: StructValue,
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: StructValue, class: Value) -> Self {
    Self { data, class }
  }
}

impl Usertype for InstanceValue {
  const ID: UsertypeId = "Instance";

  fn register(class: &mut crate::NativeClass) {
    class.define_any_getter(|this, name| {
      if let Ok(this) = this.as_instance() {
        this.data.get(name).or_else(|| {
          if let Ok(class) = this.class.as_class() {
            class.get_method(name)
          } else {
            Value::nil
          }
        })
      } else {
        Value::nil
      }
    });

    class.define_any_setter(|this, name, value| {
      self.data.set(name, value.clone());
      value
    });
  }

  fn set(&mut self, name: &str, value: Value) -> Value {
    self.data.set(name, value)
  }

  fn get(&self, name: &str) -> Value {}

  fn stringify(&self) -> String {
    format!("<instance of {}>", self.class.stringify())
  }
}
