use crate::prelude::*;
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

#[derive(Usertype, Default)]
#[uuid("1215f7a4-1b67-4387-bf00-f950bbc63743")]
pub struct StructValue {
  #[trace]
  pub members: BTreeMap<String, Value>,
}

impl StructValue {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn set(&mut self, name: impl ToString, value: Value) {
    self.members.insert(name.to_string(), value.clone());
  }
}

impl UsertypeFields for StructValue {
  fn get_field(&self, _gc: &mut Gc, field: &str) -> UsageResult<Option<Value>> {
    Ok(self.members.get(field).cloned())
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> UsageResult<()> {
    self.set(field, value);
    Ok(())
  }
}

#[methods]
impl StructValue {
  fn __str__(&self) -> String {
    format!("{}", self)
  }
}

impl Display for StructValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{{ {} }}",
      self
        .members
        .iter()
        .map(|(k, v)| format!("{}: {}", k, v))
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}
