use super::{ComplexValue, ComplexValueId, Value};
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

#[derive(Default)]
pub struct StructValue {
  pub members: BTreeMap<String, Value>,
}

impl ComplexValue for StructValue {
  const ID: ComplexValueId = "Struct";

  fn set(&mut self, name: &str, value: Value) -> Value {
    self.members.insert(name.to_string(), value.clone());
    value
  }

  fn get(&self, name: &str) -> Value {
    self.members.get(name).cloned().unwrap_or_default()
  }
}

impl Display for StructValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}",
      self
        .members
        .iter()
        .map(|(k, v)| format!("{}: {}", k, v))
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}
