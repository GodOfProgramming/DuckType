use crate::prelude::*;

pub struct LibPs;

impl LibPs {
  pub fn load(gc: &mut Gc) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      lib.set(gc, "exit", Value::native(exit)).ok();
    })
  }
}

#[native]
fn exit(_vm: &mut Vm, code: i32) -> ValueResult {
  std::process::exit(code);
}
