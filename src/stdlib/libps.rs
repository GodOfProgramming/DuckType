use crate::prelude::*;

pub struct LibPs;

impl LibPs {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      let exit = Value::native(|_vm, _env, args| {
        let exit_code = args.list.first().map(|v| v.as_i32()).flatten().unwrap_or(1);

        std::process::exit(exit_code);
      });

      lib.set("exit", exit).ok();
    })
    .into()
  }
}
