use crate::prelude::{Usertype, UsertypeId};
use std::{ops::Deref, time::Instant};

pub struct TimestampValue(Instant);

impl TimestampValue {
  pub fn new() -> Self {
    Self(Instant::now())
  }
}

impl Default for TimestampValue {
  fn default() -> Self {
    Self::new()
  }
}

impl Usertype for TimestampValue {
  const ID: UsertypeId = "Timestamp";
}

impl Deref for TimestampValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
