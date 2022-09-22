use super::{Instance, Method, NativeMethod, Object, Struct, Value};
use crate::{Env, ExecutionThread};
use std::collections::BTreeMap;

pub struct Class {
  pub name: String,
  pub initializer: Option<Value>,
  pub methods: BTreeMap<String, Value>,
  pub static_members: BTreeMap<String, Value>,
}

impl Class {
  pub fn new<N: ToString>(name: N) -> Self {
    Self {
      name: name.to_string(),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  fn call(&mut self, thread: &mut ExecutionThread, env: &mut Env, args: Vec<Value>) -> Value {
    let mut instance = self.construct();

    if let Some(initializer) = &mut self.initializer {
      if initializer.is_fn() {
        initializer.as_fn().call(thread, env, args);
        instance.into()
      } else if initializer.is_native_method() {
        initializer
          .as_native_method()
          .call(thread, env, Value::from(instance), args)
      } else {
        Value::new_err(format!("invalid type for constructor {}", initializer))
      }
    } else {
      Value::from(instance)
    }
  }

  pub fn construct(&self) -> Value {
    let mut instance = Value::from(Instance::default());

    for (name, function) in self.methods.iter() {
      if function.is_fn() {
        let method = Method::new(instance.clone(), function.as_fn().clone());
        instance
          .as_instance()
          .set_method(name.clone(), method.into());
      } else if function.is_native_fn() {
        let method = NativeMethod::new(instance.clone(), function);
        instance
          .as_instance()
          .set_method(name.clone(), method.into());
      }
    }

    instance.into()
  }

  pub fn set_initializer(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: Value) {
    self.methods.insert(name.to_string(), value);
  }

  pub fn set_method_fn<
    N: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Value, Vec<Value>) -> Value + 'static,
  >(
    &mut self,
    name: N,
    value: F,
  ) {
    self.methods.insert(
      name.to_string(),
      Value::new_native_method(&name.to_string(), value),
    );
  }

  pub fn set_static<N: ToString>(&mut self, name: N, value: Value) {
    self.static_members.insert(name.to_string(), value);
  }

  pub fn set_static_fn<
    N: AsRef<str>,
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> Value + 'static,
  >(
    &mut self,
    name: N,
    value: F,
  ) {
    self.set_static(name.as_ref(), Value::new_native_fn(name.as_ref(), value));
  }

  pub fn get_static<N: ToString>(&self, name: &N) -> Value {
    self
      .static_members
      .get(&name.to_string())
      .cloned()
      .unwrap_or(Value::nil)
  }
}

impl Object for Class {
  fn set(&mut self, name: &str, value: Value) -> Result<(), super::Error> {
    self.set_static(name, value);
    Ok(())
  }
}
