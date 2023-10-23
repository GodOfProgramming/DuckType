use crate::prelude::*;
use std::collections::BTreeMap;

#[derive(Default, Usertype)]
#[uuid("fc79ffad-9286-4188-9905-76ae73108f9e")]
pub struct ModuleValue {
  #[trace]
  pub members: BTreeMap<String, Value>,
  #[trace]
  pub env: Env,
}

impl ModuleValue {
  pub(crate) fn new() -> Self {
    Self {
      members: Default::default(),
      env: Env::default(),
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
    self.members.insert(field.to_string(), value);
    Ok(())
  }
}

#[methods]
impl ModuleValue {
  fn __res__(&self, field: &str) -> ValueResult {
    self.env.lookup(field).ok_or(ValueError::NameError(field.to_string()))
  }

  fn __dbg__(&self) -> String {
    format!("mod {:#?}", self.members)
  }
}

pub struct ModuleBuilder(ModuleValue);

impl ModuleBuilder {
  pub fn initialize(gc: &mut Gc, f: impl FnOnce(&mut Gc, &mut ModuleValue)) -> Value {
    let mut module = ModuleValue::new();
    f(gc, &mut module);
    gc.allocate(module)
  }
}
