use crate::prelude::*;
use std::time::Instant;

pub struct LibTime;

impl LibTime {
  pub fn load(gc: &mut Gc) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      let mono = LockedModule::initialize(gc, |gc, mono| {
        mono
          .set(gc, "now", Value::native(|vm, _| Ok(vm.gc.allocate(TimestampValue::new()))))
          .ok();

        mono.set(gc, "elapsed", Value::native(elapsed)).ok();
      });

      lib.set(gc, "Monotonic", mono).ok();
    })
  }
}

#[native]
fn elapsed(_vm: &mut Vm, before: &TimestampValue) -> ValueResult {
  let now = Instant::now();
  let since = now.duration_since(**before);
  Ok(Value::from(since.as_secs_f64()))
}
