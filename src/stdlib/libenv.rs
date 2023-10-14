use crate::prelude::*;

pub struct LibEnv;

impl LibEnv {
  pub fn load(args: &[String]) -> Value {
    LockedModule::initialize(|lib| {
      lib
        .set(
          "ARGV",
          Value::from(args.iter().map(|arg| Value::from(arg.clone())).collect::<Vec<Value>>()),
        )
        .ok();
    })
    .into()
  }
}
