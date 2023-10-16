use crate::prelude::*;

pub struct LibPs;

impl LibPs {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      lib.set("exit", Value::native(exit)).ok();
    })
    .into()
  }
}

#[native]
fn exit(code: i32) -> ValueResult {
  std::process::exit(code);
}
