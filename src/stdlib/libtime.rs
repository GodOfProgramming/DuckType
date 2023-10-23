use crate::prelude::*;
use std::time::Instant;

pub struct LibTime;

impl LibTime {
  pub fn load(gc: &mut Gc) -> Value {
    ModuleBuilder::initialize(gc, |gc, lib| {
      let mono = ModuleBuilder::initialize(gc, |gc, mono| {
        mono
          .set(gc, "now", Value::native(|vm, _| Ok(vm.gc.allocate(TimestampValue::new()))))
          .ok();

        mono.set(gc, "elapsed", Value::native(elapsed)).ok();
      });

      lib.set(gc, "mono", mono).ok();
    })
  }
}

#[native]
fn elapsed(before: &TimestampValue) -> ValueResult {
  let now = Instant::now();
  let since = now.duration_since(**before);
  Ok(Value::from(since.as_secs_f64()))
}
