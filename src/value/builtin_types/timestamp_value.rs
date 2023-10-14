use crate::prelude::*;
use std::{ops::Deref, time::Instant};

#[derive(Usertype, Class)]
pub struct TimestampValue {
  timestamp: Instant,
}

impl TimestampValue {
  pub fn new() -> Self {
    Self {
      timestamp: Instant::now(),
    }
  }
}

#[methods]
impl TimestampValue {}

impl Default for TimestampValue {
  fn default() -> Self {
    Self::new()
  }
}

impl Deref for TimestampValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.timestamp
  }
}
