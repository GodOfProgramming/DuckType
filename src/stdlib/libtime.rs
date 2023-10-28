use crate::prelude::*;
use std::time::Instant;

pub fn mono(_: &mut SmartPtr<Gc>, mut mono: UsertypeHandle<ModuleValue>) {
  mono.define("now", Value::native(now));
  mono.define("elapsed", Value::native(elapsed));
}

#[native]
fn now() -> ValueResult<TimestampValue> {
  Ok(TimestampValue::new())
}

#[native]
fn elapsed(before: &TimestampValue) -> ValueResult {
  let now = Instant::now();
  let since = now.duration_since(**before);
  Ok(Value::from(since.as_secs_f64()))
}
