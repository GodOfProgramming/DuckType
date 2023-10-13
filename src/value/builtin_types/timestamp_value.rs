use macros::{class_body, Class};

use crate::prelude::*;
use std::{ops::Deref, time::Instant};

#[derive(Class)]
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

#[class_body]
impl TimestampValue {}

impl Default for TimestampValue {
  fn default() -> Self {
    Self::new()
  }
}

impl Usertype for TimestampValue {
  const ID: &'static str = "Timestamp";
}

impl Deref for TimestampValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.timestamp
  }
}
