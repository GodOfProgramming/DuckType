use super::{Error, Object, Value};
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

#[derive(Default)]
pub struct Struct {
  pub members: BTreeMap<String, Value>,
}

impl Object for Struct {
  fn set(&mut self, name: &str, value: Value) -> Result<(), Error> {
    self.members.insert(name.to_string(), value);
    Ok(())
  }

  fn get(&self, name: &str) -> Value {
    self.members.get(name).cloned().unwrap_or_default()
  }
}

impl Display for Struct {
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
