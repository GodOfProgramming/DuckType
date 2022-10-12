use crate::prelude::{ComplexValue, ComplexValueId};
use std::{ops::Deref, time::Instant};

pub struct TimestampValue(Instant);

impl TimestampValue {
  pub fn new() -> Self {
    Self(Instant::now())
  }
}

impl ComplexValue for TimestampValue {
  const ID: ComplexValueId = "Timestamp";
}

impl Deref for TimestampValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
