use crate::prelude::*;
use std::time::Instant;

pub struct LibTime;

impl LibTime {
  pub fn load(gc: &mut SmartPtr<Gc>, gmod: Value) -> UsertypeHandle<ModuleValue> {
    ModuleBuilder::initialize(gc, Some(gmod), |gc, mut lib| {
      let mono = ModuleBuilder::initialize(gc, Some(lib.handle.value.clone()), |_, mut mono| {
        mono.define("now", Value::native(now));

        mono.define("elapsed", Value::native(elapsed));
      });

      lib.define("mono", mono);
    })
  }
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
