use crate::{prelude::*, FastHashMap};

#[derive(Default, Usertype, NoMethods)]
#[uuid("2034facf-835a-495c-b504-26efc0ca3f95")]
pub struct ClassValue {
  pub name: String,
  #[trace]
  pub initializer: Option<Value>,
  #[trace]
  pub methods: FastHashMap<String, FunctionValue>,
  #[trace]
  pub static_members: FastHashMap<String, Value>,
}

impl ClassValue {
  pub fn new(name: impl ToString) -> Self {
    Self {
      name: name.to_string(),
      initializer: None,
      methods: Default::default(),
      static_members: Default::default(),
    }
  }

  pub fn set_constructor(&mut self, value: Value) {
    self.initializer = Some(value);
  }

  pub fn get_method(&self, vm: &mut Vm, this: Value, field: Field) -> Option<Value> {
    let method = field.name(vm).and_then(|name| self.methods.get(name).cloned());
    method.map(|method| vm.make_value_from(MethodValue::new(this, method)))
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
  fn get_field(&self, vm: &mut Vm, field: Field) -> UsageResult<Option<Value>> {
    field
      .name(vm)
      .ok_or(UsageError::InvalidConstantIdentifier)
      .map(|name| self.get_static(name))
  }

  fn set_field(&mut self, vm: &mut Vm, field: Field, value: Value) -> UsageResult<()> {
    field
      .name(vm)
      .ok_or(UsageError::InvalidConstantIdentifier)
      .map(|name| self.set_static(name, value))
  }
}

impl Operators for ClassValue {
  fn __ivk__(&mut self, vm: &mut Vm, class: Value, airity: usize) -> UsageResult {
    let self_type = vm.make_value_from(StructValue::default());

    let instance = vm.make_value_from(InstanceValue::new(self_type, class));

    if let Some(initializer) = &mut self.initializer {
      vm.stack_push(instance);
      initializer.call(vm, airity + 1)
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
