use crate::{ComplexValue, StructValue, Value};

pub struct LibPs;

impl LibPs {
  pub fn load() -> Value {
    let mut lib = StructValue::default();

    let exit = Value::new_native_fn(|_thread, _env, args| {
      let exit_code = args.list.first().map(|v| v.as_i32().unwrap_or(0)).unwrap_or(0);

      std::process::exit(exit_code);
    });

    lib.set("exit", exit).ok();

    Value::from(lib)
  }
}