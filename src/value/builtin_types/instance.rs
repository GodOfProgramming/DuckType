use crate::{Env, ExecutionThread};

use super::{Method, Object, Struct, UnimplementedFunction, Value};
use std::collections::BTreeMap;

#[derive(Default)]
pub struct Instance {
  pub data: Struct,
  methods: BTreeMap<String, Value>,
}

impl Instance {
  pub fn new(data: Struct, class: Value) -> Self {
    Self {
      data,
      methods: BTreeMap::default(),
    }
  }

  pub fn set<N: ToString>(&mut self, name: N, value: Value) {
    self.data.set(&name.to_string(), value);
  }

  pub fn get<N: ToString + ?Sized>(&self, name: &N) -> Value {
    self.data.get(&name.to_string()).or_else(|| {
      self
        .methods
        .get(&name.to_string())
        .cloned()
        .map(|m| Value::from(m))
        .unwrap_or_default()
    })
  }

  pub fn set_method<N: ToString>(&mut self, name: N, method: Value) {
    self.methods.insert(name.to_string(), method);
  }

  pub fn call_method<N: ToString>(
    &mut self,
    name: N,
    thread: &mut ExecutionThread,
    env: &mut Env,
    this: Value,
    args: Vec<Value>,
  ) {
    if let Some(method) = self.methods.get_mut(&name.to_string()) {
      if method.is_method() {
        let method = method.as_method();
        method.call(thread, env, args);
      } else if method.is_native_method() {
        let method = method.as_native_method();
        thread.stack_push(method.call(thread, env, args));
      }
    } else {
      thread.stack_push(
        UnimplementedFunction::Custom(name.to_string())
          .to_string()
          .into(),
      )
    }
  }
}

impl Object for Instance {}
