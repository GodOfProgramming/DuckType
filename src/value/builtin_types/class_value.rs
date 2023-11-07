use crate::prelude::*;
use ahash::RandomState;
use std::collections::HashMap;

#[derive(Default, Usertype)]
#[uuid("2034facf-835a-495c-b504-26efc0ca3f95")]
pub struct ClassValue {
  pub name: String,
  #[trace]
  pub creator: Value,
  #[trace]
  pub initializer: Option<Value>,
  #[trace]
  pub methods: HashMap<String, FunctionValue, RandomState>,
  #[trace]
  pub static_members: HashMap<String, Value, RandomState>,
}

impl ClassValue {
  pub fn new(name: impl ToString, creator: Value) -> Self {
    Self {
      creator,
      name: name.to_string(),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  pub fn set_constructor(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn get_method(&self, gc: &mut Gc, this: &Value, field: Field) -> Option<Value> {
    field.name.and_then(|name| {
      self
        .methods
        .get(name)
        .cloned()
        .map(|method| gc.allocate(MethodValue::new(this.clone(), method)))
    })
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
  fn get_field(&self, _gc: &mut Gc, field: Field) -> UsageResult<Option<Value>> {
    Ok(field.name.and_then(|name| self.get_static(name)))
  }

  fn set_field(&mut self, _gc: &mut Gc, field: Field, value: Value) -> UsageResult<()> {
    if let Some(name) = field.name {
      self.set_static(name, value);
      Ok(())
    } else {
      Err(UsageError::EmptyField)
    }
  }
}

#[methods]
impl ClassValue {
  fn __ivk__(&mut self, vm: &mut Vm, class: Value, args: Args) -> UsageResult {
    let self_type = self.creator.call(vm, Args::default())?;

    let instance = vm.gc.allocate(InstanceValue::new(self_type, class.clone()));

    if let Some(initializer) = &mut self.initializer {
      if let Some(initializer) = initializer.as_fn_mut() {
        let args = Args::new_with_this(instance.clone(), args.list);
        initializer.__ivk__(vm, Value::nil, args)
      } else {
        Err(UsageError::MethodType)
      }
    } else {
      Ok(instance)
    }
  }

  fn __str__(&self) -> String {
    format!("<class {}>", self.name)
  }

  fn __dbg__(&self) -> String {
    format!("class {}", self.__str__())
  }
}
