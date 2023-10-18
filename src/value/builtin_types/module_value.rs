use crate::{
  memory::{Allocation, Gc},
  prelude::*,
};
use std::collections::BTreeMap;

#[derive(Default, Usertype)]
#[uuid("fc79ffad-9286-4188-9905-76ae73108f9e")]
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

  pub fn set(&mut self, gc: &mut Gc, field: &str, value: impl Into<Value>) -> ValueResult<()> {
    <Self as Usertype>::set(self, gc, field, value.into())
  }
}

impl UsertypeFields for ModuleValue {
  fn get_field(&self, _gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
    self
      .members
      .get(field)
      .cloned()
      .map(Ok)
      .or_else(|| Some(Err(ValueError::UndefinedMember(field.to_string()))))
      .transpose()
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
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

  fn __dbg__(&self) -> String {
    format!("mod {:#?}", self.members)
  }
}

pub struct LockedModule(ModuleValue);

impl LockedModule {
  pub fn initialize(gc: &mut Gc, f: impl FnOnce(&mut Gc, &mut ModuleValue)) -> Value {
    let mut module = ModuleValue::new();
    f(gc, &mut module);
    module.__lock__();
    gc.allocate(module)
  }
}
