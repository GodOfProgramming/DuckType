use crate::{prelude::*, UnwrapAnd};
use ahash::RandomState;
use bimap::BiHashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

type KeyValPair<K> = ((K, BitsRepr), Value);

pub struct StructMember {
  id: BitsRepr,
  value: Value,
}

impl PartialEq for StructMember {
  fn eq(&self, other: &Self) -> bool {
    self.id == other.id
  }
}

impl Eq for StructMember {}

impl PartialOrd for StructMember {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for StructMember {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.id.cmp(&other.id)
  }
}

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
  pub members: Vec<StructMember>,
  pub string_ids: BiHashMap<String, BitsRepr, RandomState, RandomState>,
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
    let mut members = Vec::default();
    let mut string_ids = BiHashMap::with_hashers(RandomState::new(), RandomState::new());

    for ((string, id), value) in member_iter.into_iter() {
      members.push(StructMember { id, value });
      string_ids.insert(string.to_string(), id);
    }

    members.sort_by_key(|m| m.id);

    Self { members, string_ids }
  }

  pub(crate) fn sort_members(&mut self) {
    self.members.sort_by_key(|m| m.id);
  }

  fn set_mem(&mut self, idx: usize, value: Value) {
    self.members[idx].value = value;
  }

  fn get_idx_by_id(&self, id: &BitsRepr) -> Option<usize> {
    self.members.binary_search_by_key(id, |m| m.id).ok()
  }

  fn get_field_by_id(&self, id: &BitsRepr) -> Option<Value> {
    self
      .members
      .binary_search_by_key(id, |m| m.id)
      .ok()
      .map(|idx| self.members[idx].value.clone())
  }

  fn get_id_by_field(&self, field: Field) -> Result<BitsRepr, UsageError> {
    field
      .id
      .as_ref()
      .or_else(|| field.name.and_then(|name| self.string_ids.get_by_left(name)))
      .cloned()
      .ok_or(UsageError::EmptyField)
  }
}

impl UsertypeFields for StructValue {
  fn get_field(&self, _gc: &mut Gc, field: Field) -> UsageResult<Option<Value>> {
    self.get_id_by_field(field).map(|id| self.get_field_by_id(&id))
  }

  fn set_field(&mut self, _gc: &mut Gc, field: Field, value: Value) -> UsageResult<()> {
    self
      .get_id_by_field(field)
      .map(|id| self.get_idx_by_id(&id).unwrap_and(|idx| self.set_mem(idx, value)))
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
        .map(|m| format!("{}: {}", self.string_ids.get_by_right(&m.id).unwrap(), m.value))
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}

impl TraceableValue for StructMember {
  fn trace(&self, marks: &mut Marker) {
    marks.trace(&self.value);
  }
}

impl TraceableValue for Vec<StructMember> {
  fn trace(&self, marks: &mut Marker) {
    for member in self {
      member.trace(marks);
    }
  }
}
