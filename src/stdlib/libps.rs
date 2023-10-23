use crate::prelude::*;

pub struct LibPs;

impl LibPs {
  pub fn load(gc: &mut Gc) -> Value {
    ModuleBuilder::initialize(gc, |gc, lib| {
      lib.set(gc, "exit", Value::native(exit)).ok();
    })
  }
}

#[native]
fn exit(code: i32) -> ValueResult {
  std::process::exit(code);
}
