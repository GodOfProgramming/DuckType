use crate::prelude::*;

#[derive(Usertype)]
#[uuid("988a6bd1-4a54-416f-aad5-0d1cc8ce652e")]
pub struct InstanceValue {
  #[trace]
  pub data: Value,
  #[trace]
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: Value, class: Value) -> Self {
    Self { data, class }
  }
}

impl UsertypeFields for InstanceValue {
  fn get_field(&self, gc: &mut Gc, field: Field) -> UsageResult<Option<Value>> {
    self.data.get_member(gc, field)
  }

  fn set_field(&mut self, gc: &mut Gc, field: Field, value: Value) -> UsageResult<()> {
    self.data.set_member(gc, field, value)
  }
}

impl UsertypeMethods for InstanceValue {
  fn get_method(&self, gc: &mut Gc, this: &Value, field: Field) -> UsageResult<Option<Value>> {
    if let Some(class) = self.class.cast_to::<ClassValue>() {
      Ok(class.get_method(gc, this, field))
    } else {
      Ok(None)
    }
  }
}

impl DisplayValue for InstanceValue {
  fn __str__(&self) -> String {
    format!("<instance of {}>", self.class)
  }
}

impl DebugValue for InstanceValue {
  fn __dbg__(&self) -> String {
    self.__str__()
  }
}

impl ResolvableValue for InstanceValue {}

impl InvocableValue for InstanceValue {
  fn __ivk__(&mut self, vm: &mut Vm, this: Value, airity: usize) -> UsageResult {
    let mut callable = self
      .get(&mut vm.gc, &this, Field::named("__ivk__"))?
      .map(Ok)
      .unwrap_or(Err(UsageError::UnexpectedNil))?;

    callable.call(vm, airity)
  }
}
