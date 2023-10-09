use crate::prelude::*;
use macros::Class;
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

#[derive(Default, Class)]
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

  pub fn get(&self, name: &str) -> Value {
    self.members.get(name).cloned().unwrap_or_default()
  }
}

impl Usertype for StructValue {
  const ID: &'static str = "Struct";

  fn stringify(&self) -> String {
    self.to_string()
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
