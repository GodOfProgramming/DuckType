use crate::prelude::*;
use uuid::Uuid;

#[derive(Clone, Usertype, Fields, NoMethods)]
#[uuid("2ed77c70-1b5a-4dcd-92dc-865078cbb997")]
pub struct IdValue {
  pub id: Uuid,
}

impl IdValue {
  pub fn new(id: Uuid) -> Self {
    Self { id }
  }
}

impl Operators for IdValue {
  #[binary]
  fn __eq__(left: &IdValue, right: Value) -> UsageResult<bool> {
    Ok(right.cast_to::<IdValue>().map(|right| left.id == right.id).unwrap_or(false))
  }

  fn __str__(&self) -> String {
    self.id.to_string()
  }

  fn __dbg__(&self) -> String {
    self.id.to_string()
  }
}
