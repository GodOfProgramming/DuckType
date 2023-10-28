use crate::prelude::*;

pub struct LibPs;

pub fn ps(gc: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("exit", Value::native(exit));
}

#[native]
fn exit(code: i32) -> ValueResult {
  std::process::exit(code);
}
