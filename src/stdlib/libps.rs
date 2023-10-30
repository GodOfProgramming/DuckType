use std::time::Duration;

use crate::prelude::*;

pub fn ps(gc: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("exit", Value::native(exit));
  super::defmod(gc, &mut lib, "thread", |_, mut lib| {
    lib.define("sleep", Value::native(thread_sleep));
  });
}

#[native]
fn exit(code: i32) -> ValueResult {
  std::process::exit(code);
}

#[native]
fn thread_sleep(seconds: f64) -> ValueResult<()> {
  std::thread::sleep(Duration::from_secs_f64(seconds));
  Ok(())
}
