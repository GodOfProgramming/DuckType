use crate::prelude::*;
use std::time::Instant;

pub struct LibTime;

impl LibTime {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      let mono = LockedModule::initialize(|mono| {
        mono
          .set("now", Value::native(|_vm, _args| Ok(Value::from(TimestampValue::new()))))
          .ok();

        mono.set("elapsed", Value::native(elapsed)).ok();
      });

      lib.set("Monotonic", Value::from(mono)).ok();
    })
    .into()
  }
}

#[native]
fn elapsed(before: &TimestampValue) -> ValueResult {
  let now = Instant::now();
  let since = now.duration_since(**before);
  Ok(Value::from(since.as_secs_f64()))
}
