use crate::prelude::*;
use std::{ops::Deref, time::Instant};

#[derive(Usertype, Fields)]
#[uuid("fa23bba8-599b-4626-98c2-5036ecf49265")]
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
