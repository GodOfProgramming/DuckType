use crate::prelude::*;
use ahash::RandomState;
use bimap::BiHashMap;
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter, Result as FmtResult},
};

type KeyValPair<K> = ((K, usize), Value);

/// A value representing an anonymous struct
#[derive(Default, Usertype, NoMethods)]
#[uuid("1215f7a4-1b67-4387-bf00-f950bbc63743")]
pub struct StructValue {
  #[trace]
  members: BTreeMap<usize, Value>,
  string_ids: BiHashMap<String, usize, RandomState, RandomState>,
}

impl StructValue {
  pub fn empty() -> Self {
    Self {
      members: Default::default(),
      string_ids: Default::default(),
    }
  }

  pub fn new<K>(member_iter: impl IntoIterator<Item = KeyValPair<K>>) -> Self
  where
    K: ToString,
  {
    let mut members = BTreeMap::default();
    let mut string_ids = BiHashMap::with_hashers(RandomState::new(), RandomState::new());

    for ((string, id), value) in member_iter.into_iter() {
      members.insert(id, value);
      string_ids.insert(string.to_string(), id);
    }

    Self { members, string_ids }
  }

  fn set_mem(&mut self, idx: usize, value: Value) {
    self.members.insert(idx, value);
  }

  fn get_mem_by_id(&self, id: &usize) -> Option<Value> {
    self.members.get(id).cloned()
  }

  /// Unknown string ids by this struct indicate the struct's field was never set
  fn resolve_id(&self, field: Field) -> Option<usize> {
    match field {
      Field::Id(id) => Some(id),
      Field::Named(name) => self.string_ids.get_by_left(name).cloned(),
      Field::NamedId(id, _) => Some(id),
    }
  }

  /// Looks up the field id, and saves it if it's not found
  fn save_resolved_id(&mut self, vm: &mut Vm, field: Field) -> usize {
    match field {
      Field::Id(id) => id,
      Field::Named(name) => self.string_ids.get_by_left(name).cloned().unwrap_or_else(|| {
        let id = vm.id_of(name);
        self.string_ids.insert(name.to_string(), id);
        id
      }),
      Field::NamedId(id, _) => id,
    }
  }
}

impl UsertypeFields for StructValue {
  fn get_field(&self, _: &mut Vm, field: Field) -> UsageResult<Option<Value>> {
    Ok(self.resolve_id(field).and_then(|id| self.get_mem_by_id(&id)))
  }

  fn set_field(&mut self, vm: &mut Vm, field: Field, value: Value) -> UsageResult<()> {
    let id = self.save_resolved_id(vm, field);
    self.set_mem(id, value);
    Ok(())
  }
}

impl Operators for StructValue {
  fn __str__(&self) -> String {
    format!("{self}")
  }

  fn __dbg__(&self) -> String {
    format!("{self}")
  }
}

impl Display for StructValue {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "struct {{ {} }}",
      self
        .members
        .iter()
        .map(|(id, value)| {
          if value.pointer_t::<Self>() == &raw const *self {
            format!("{}: {}", self.string_ids.get_by_right(id).unwrap(), "<self>")
          } else {
            format!("{}: {}", self.string_ids.get_by_right(id).unwrap(), value)
          }
        })
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}
