use crate::prelude::*;
use std::{ops::Deref, time::Instant};

pub fn mono(_: &mut SmartPtr<Gc>, mut mono: UsertypeHandle<ModuleValue>) {
  mono.define("now", Value::native(now));
  mono.define("elapsed", Value::native(elapsed));
}

#[native]
fn now() -> UsageResult<TimeValue> {
  Ok(TimeValue::new())
}

#[native]
fn elapsed(before: &TimeValue) -> UsageResult<f64> {
  let now = Instant::now();
  let since = now.duration_since(**before);
  Ok(since.as_secs_f64())
}

#[derive(Usertype, Fields)]
#[uuid("fa23bba8-599b-4626-98c2-5036ecf49265")]
pub struct TimeValue {
  timestamp: Instant,
}

impl TimeValue {
  pub fn new() -> Self {
    Self {
      timestamp: Instant::now(),
    }
  }
}

#[methods]
impl TimeValue {
  fn __eq__(&self, other: Value) -> UsageResult<bool> {
    Ok(
      other
        .cast_to::<Self>()
        .map(|other| self.timestamp == other.timestamp)
        .unwrap_or(false),
    )
  }

  fn __neq__(&self, other: Value) -> UsageResult<bool> {
    self.__eq__(other).map(|v| !v)
  }
}

impl Default for TimeValue {
  fn default() -> Self {
    Self::new()
  }
}

impl Deref for TimeValue {
  type Target = Instant;

  fn deref(&self) -> &Self::Target {
    &self.timestamp
  }
}
