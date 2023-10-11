use macros::{class_body, Class};

use crate::{code::ClassConstant, prelude::*};
use std::collections::BTreeMap;

#[derive(Class)]
pub struct ClassValue {
  pub name: String,
  pub initializer: Option<Value>,
  pub methods: BTreeMap<String, Value>,
  pub static_members: BTreeMap<String, Value>,
}

impl ClassValue {
  pub fn new<N: ToString>(name: N) -> Self {
    Self {
      name: name.to_string(),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  pub fn construct(mut class_value: Value, vm: &mut Vm, mut args: Args) -> ValueResult<()> {
    let class_clone = class_value.clone();
    if let Some(class) = class_value.as_class_mut() {
      let instance = Value::from(InstanceValue::new(StructValue::default(), class_clone));
      if let Some(initializer) = &mut class.initializer {
        if let Some(initializer) = initializer.as_fn() {
          args.list.push(instance);
          initializer.call(vm, args.list);
        } else {
          Err(ValueError::Todo(format!("invalid type for constructor {}", initializer)))?;
        }
      } else {
        vm.stack_push(instance);
      }
    } else {
      Err(ValueError::Todo("unable to construct instance from non-class".to_string()))?;
    };
    Ok(())
  }

  pub fn set_constructor(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn get_method(&self, this: Value, name: &str) -> Value {
    self.methods.get(name).cloned().unwrap_or_default()
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: Value) {
    self.methods.insert(name.to_string(), value);
  }

  pub fn get_static(&self, name: &str) -> Value {
    self.static_members.get(name).cloned().unwrap_or_default()
  }

  pub fn set_static<N: ToString>(&mut self, name: N, value: Value) {
    self.static_members.insert(name.to_string(), value);
  }
}

#[class_body]
impl ClassValue {}

impl Usertype for ClassValue {
  const ID: &'static str = "Class";

  fn stringify(&self) -> String {
    self.name.clone()
  }

  fn debug_string(&self) -> String {
    format!("class {}", self.stringify())
  }
}

impl From<&ClassConstant> for ClassValue {
  fn from(c: &ClassConstant) -> Self {
    Self {
      name: c.name.clone(),
      initializer: c.initializer.as_ref().map(|i| Value::from(FunctionValue::from(i))),
      methods: c
        .methods
        .iter()
        .map(|(k, v)| (k.clone(), Value::from(MethodValue::new(FunctionValue::from(v)))))
        .collect(),
      static_members: c
        .statics
        .iter()
        .map(|(k, v)| (k.clone(), Value::from(FunctionValue::from(v))))
        .collect(),
    }
  }
}
