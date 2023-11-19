use crate::prelude::*;
use std::{ops::Deref, time::Instant};

pub fn mono(_: &mut SmartPtr<Gc>, mut mono: UsertypeHandle<ModuleValue>) {
  mono.define("now", Value::new::<NativeFn>(now));
  mono.define("elapsed", Value::new::<NativeFn>(elapsed));
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

#[derive(Usertype, Fields, NoMethods)]
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

impl Operators for TimeValue {
  #[binary]
  fn __eq__(left: &TimeValue, right: Value) -> UsageResult<bool> {
    Ok(
      right
        .cast_to::<TimeValue>()
        .map(|right| left.timestamp == right.timestamp)
        .unwrap_or(false),
    )
  }

  #[binary]
  fn __neq__(left: &TimeValue, right: Value) -> UsageResult<bool> {
    Ok(
      right
        .cast_to::<TimeValue>()
        .map(|right| left.timestamp != right.timestamp)
        .unwrap_or(true),
    )
  }

  fn __str__(&self) -> String {
    format!("{:?}", self.timestamp)
  }

  fn __dbg__(&self) -> String {
    format!("{:?}", self.timestamp)
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
