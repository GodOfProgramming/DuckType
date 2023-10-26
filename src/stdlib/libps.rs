use crate::prelude::*;

pub struct LibPs;

impl LibPs {
  pub fn load(gc: &mut SmartPtr<Gc>, gmod: Value) -> UsertypeHandle<ModuleValue> {
    ModuleBuilder::initialize(gc, Some(gmod), |_, mut lib| {
      lib.define("exit", Value::native(exit));
    })
  }
}

#[native]
fn exit(code: i32) -> ValueResult {
  std::process::exit(code);
}
