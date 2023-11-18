use std::time::Duration;

use crate::prelude::*;

pub fn ps(gc: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("exit", Value::new::<NativeFn>(exit));
  super::defmod(gc, &mut lib, "thread", |_, mut lib| {
    lib.define("sleep", Value::new::<NativeFn>(thread_sleep));
  });
}

#[native]
fn exit(code: i32) -> UsageResult {
  std::process::exit(code);
}

#[native]
fn thread_sleep(seconds: f64) -> UsageResult<()> {
  std::thread::sleep(Duration::from_secs_f64(seconds));
  Ok(())
}
