use crate::{code::ClassConstant, prelude::*};
use itertools::Itertools;
use std::collections::BTreeMap;

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

  pub fn construct(mut class_value: Value, vm: &mut Vm, env: &mut Env, mut args: Args) {
    let class_clone = class_value.clone();
    if let Ok(class) = class_value.as_class_mut() {
      let instance = Value::from(InstanceValue::new(StructValue::default(), class_clone));
      if let Some(initializer) = &mut class.initializer {
        if let Ok(initializer) = initializer.as_fn() {
          args.list.push(instance);
          initializer.call(vm, args.list);
        } else {
          vm.stack_push(Value::new_err(format!("invalid type for constructor {}", initializer)));
        }
      } else {
        vm.stack_push(instance);
      }
    } else {
      vm.stack_push(Value::new_err("unable to construct instance from non-class"));
    };
  }

  pub fn set_constructor(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn set_native_constructor_fn(&mut self, f: fn(&mut Vm, &mut Env, Args) -> Value) {
    self.initializer = Some(Value::new_native_fn(f));
  }

  pub fn set_native_constructor_closure<F>(&mut self, f: F)
  where
    F: FnMut(&mut Vm, &mut Env, Args) -> Value + 'static,
  {
    self.initializer = Some(Value::new_native_closure(format!("{}()", self.name), f));
  }

  pub fn get_method(&self, name: &str) -> Value {
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

impl Usertype for ClassValue {
  const ID: UsertypeId = "Class";

  fn register(class: &mut NativeClassBuilder<Self>) {
    class.add_getter("methods", |this| {
      ArrayValue::new_from_vec(this.methods.keys().map(|s| Value::from(s)).collect_vec()).into()
    });

    class.add_getter("statics", |this| {
      ArrayValue::new_from_vec(this.static_members.keys().map(|s| Value::from(s)).collect_vec()).into()
    });
  }

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
