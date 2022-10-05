use crate::prelude::ComplexValue;
use std::{ops::Deref, time::Instant};

pub struct TimestampValue(Instant);

impl TimestampValue {
  pub fn new() -> Self {
    Self(Instant::now())
  }
}

impl ComplexValue for TimestampValue {}

impl Deref for TimestampValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
