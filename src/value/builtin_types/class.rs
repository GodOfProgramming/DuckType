use super::{Args, ComplexValue, InstanceValue, Value};
use crate::{Env, ExecutionThread, StructValue};
use std::{collections::BTreeMap, fmt::Display};

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

  pub fn call_constructor(
    mut class_value: Value,
    thread: &mut ExecutionThread,
    env: &mut Env,
    args: Args,
  ) {
    let class_clone = class_value.clone();
    let result = if let Ok(class) = class_value.as_class_mut() {
      let instance = Value::from(InstanceValue::new(StructValue::default(), class_clone));
      if let Some(initializer) = &mut class.initializer {
        if let Ok(initializer) = initializer.as_fn() {
          initializer.call(thread, args);
          instance
        } else if let Ok(initializer) = initializer.as_native_fn() {
          initializer(thread, env, args)
        } else if let Ok(initializer) = initializer.as_native_closure_mut() {
          initializer.call(thread, env, args)
        } else {
          Value::new_err(format!("invalid type for constructor {}", initializer))
        }
      } else {
        instance
      }
    } else {
      Value::new_err("unable to construct instance from non-class")
    };

    thread.stack_push(result);
  }

  pub fn set_constructor(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn set_native_constructor<F>(&mut self, f: F)
  where
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    self.initializer = Some(Value::new_native_closure(format!("{}()", self.name), f));
  }

  pub fn get_method<N: AsRef<str>>(&self, name: N) -> Value {
    self.methods.get(name.as_ref()).cloned().unwrap_or_default()
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: Value) {
    self.methods.insert(name.to_string(), value);
  }

  pub fn set_native_method<N, F>(&mut self, name: N, method: F)
  where
    N: Display,
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    self.methods.insert(
      name.to_string(),
      Value::new_native_closure(format!("{}.{}", self.name, name), method),
    );
  }

  pub fn get_static<N: AsRef<str>>(&self, name: N) -> Value {
    self
      .static_members
      .get(name.as_ref())
      .cloned()
      .unwrap_or_default()
  }

  pub fn set_static<N: ToString>(&mut self, name: N, value: Value) {
    self.static_members.insert(name.to_string(), value);
  }

  pub fn set_native_static<N, F>(&mut self, name: N, method: F)
  where
    N: Display,
    F: FnMut(&mut ExecutionThread, &mut Env, Args) -> Value + 'static,
  {
    self.static_members.insert(
      name.to_string(),
      Value::new_native_closure(format!("{}.{}", self.name, name), method),
    );
  }
}

impl ComplexValue for ClassValue {
  fn set(&mut self, name: &str, value: Value) -> Result<(), super::ErrorValue> {
    self.set_static(name, value);
    Ok(())
  }

  fn get(&self, name: &str) -> Value {
    self.get_static(name)
  }
}
