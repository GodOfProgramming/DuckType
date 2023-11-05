use ahash::RandomState;

use crate::prelude::*;
use std::{
  collections::HashMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

type KeyValPair<K> = (K, Value);

/// A value representing an anonymous struct
///
/// Values are stored in a map, and new fields cannot
/// be directly added through usage for performance
/// reasons
///
///
/// Instead they must be added through native functions
/// that have direct access to the internal table
#[derive(Usertype)]
#[uuid("1215f7a4-1b67-4387-bf00-f950bbc63743")]
pub struct StructValue {
  #[trace]
  pub members: HashMap<String, Value, RandomState>,
}

impl StructValue {
  pub fn empty() -> Self {
    Self {
      members: Default::default(),
    }
  }

  pub fn new<K>(members: impl IntoIterator<Item = KeyValPair<K>>) -> Self
  where
    K: ToString,
  {
    let mut map = HashMap::with_hasher(RandomState::new());

    for (key, value) in members.into_iter() {
      map.insert(key.to_string(), value);
    }

    Self { members: map }
  }
}

impl UsertypeFields for StructValue {
  fn get_field(&self, _gc: &mut Gc, field: &str) -> UsageResult<Option<Value>> {
    Ok(self.members.get(field).cloned())
  }

  fn set_field(&mut self, _gc: &mut Gc, field: &str, value: Value) -> UsageResult<()> {
    self.members.insert(field.to_string(), value.clone());
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
