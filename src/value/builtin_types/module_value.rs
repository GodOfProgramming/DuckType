use crate::prelude::*;
use std::collections::BTreeMap;

#[derive(Usertype)]
pub struct ModuleValue {
  pub members: BTreeMap<String, Value>,
  pub locked: bool,
}

impl ModuleValue {
  pub(crate) fn new() -> Self {
    Self {
      members: Default::default(),
      locked: false,
    }
  }
}

impl ClassFields for ModuleValue {
  fn get_member(&self, field: &str) -> Option<Value> {
    self.members.get(field).cloned()
  }

  fn set_member(&mut self, field: &str, value: Value) -> ValueResult<()> {
    if self.locked {
      Err(ValueError::Immutable(self.__str__()))
    } else {
      self.members.insert(field.to_string(), value);
      Ok(())
    }
  }
}

#[methods]
impl ModuleValue {
  fn __lock__(&mut self) {
    self.locked = true;
  }
}

pub struct LockedModule(ModuleValue);

impl LockedModule {
  pub fn initialize(f: impl FnOnce(&mut ModuleValue)) -> ModuleValue {
    let mut module = ModuleValue::new();
    f(&mut module);
    module.__lock__();
    module
  }
}

impl Into<Value> for LockedModule {
  fn into(self) -> Value {
    self.0.into()
  }
}
