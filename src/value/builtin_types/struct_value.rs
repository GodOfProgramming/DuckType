use crate::prelude::*;
use macros::{class_body, Class};
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

#[derive(Default)]
pub struct StructValue {
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

#[class_body]
impl StructValue {}

impl Usertype for StructValue {
  const ID: &'static str = "Struct";

  fn stringify(&self) -> String {
    self.to_string()
  }
}

impl Class for StructValue {
  fn id(&self) -> &'static str {
    "StructValue"
  }

  fn get_member(&self, field: &str) -> Option<Value> {
    self.members.get(field).cloned()
  }

  fn set_member(&mut self, field: &str, value: Value) -> ValueResult<()> {
    self.set(field, value);
    Ok(())
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
