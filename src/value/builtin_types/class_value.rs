use crate::{code::ClassConstant, prelude::*};
use std::collections::BTreeMap;

#[derive(Usertype)]
#[uuid("2034facf-835a-495c-b504-26efc0ca3f95")]
pub struct ClassValue {
  pub name: Option<String>,
  pub initializer: Option<Value>,
  pub methods: BTreeMap<String, FunctionValue>,
  pub static_members: BTreeMap<String, Value>,
}

impl ClassValue {
  pub fn new<N: ToString>(name: N) -> Self {
    Self {
      name: Some(name.to_string()),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  pub fn from_constant(gc: &mut Gc, c: &ClassConstant) -> Value {
    let class = Self {
      name: c.name.clone(),
      initializer: c.initializer.as_ref().map(|i| gc.allocate(FunctionValue::from(i))),
      methods: c.methods.iter().map(|(k, v)| (k.clone(), FunctionValue::from(v))).collect(),
      static_members: c
        .statics
        .iter()
        .map(|(k, v)| (k.clone(), gc.allocate(FunctionValue::from(v))))
        .collect(),
    };
    gc.allocate(class)
  }

  pub fn construct(vm: &mut Vm, mut class_value: Value, mut args: Args) -> ValueResult<()> {
    let class_clone = class_value.clone();
    if let Some(class) = class_value.as_class_mut() {
      let instance = vm.gc.allocate(InstanceValue::new(StructValue::default(), class_clone));
      if let Some(initializer) = &mut class.initializer {
        if let Some(initializer) = initializer.as_fn() {
          args.list.push(instance);
          initializer.call(vm, args);
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

  pub fn get_method(&self, gc: &mut Gc, this: &Value, name: &str) -> Option<Value> {
    self
      .methods
      .get(name)
      .cloned()
      .map(|method| gc.allocate(MethodValue::new(this.clone(), method)))
  }

  pub fn set_method<N: ToString>(&mut self, name: N, value: FunctionValue) {
    self.methods.insert(name.to_string(), value);
  }

  pub fn get_static(&self, name: &str) -> Option<Value> {
    self.static_members.get(name).cloned()
  }

  pub fn set_static<N: ToString>(&mut self, name: N, value: Value) {
    self.static_members.insert(name.to_string(), value);
  }
}

impl UsertypeFields for ClassValue {
  fn get_field(&self, _gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
    Ok(self.get_static(field))
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
    self.set_static(field, value);
    Ok(())
  }
}

#[methods]
impl ClassValue {
  fn __str__(&self) -> String {
    if let Some(name) = &self.name {
      format!("<class {}>", name.clone())
    } else {
      "<anonymous class>".to_string()
    }
  }

  fn __dbg__(&self) -> String {
    format!("class {}", self.__str__())
  }
}
