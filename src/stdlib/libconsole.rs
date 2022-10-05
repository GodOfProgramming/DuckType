use crate::{ComplexValue, StructValue, Value};

pub struct LibConsole;

impl LibConsole {
  pub fn load() -> Value {
    let mut lib = StructValue::default();

    let write = Value::new_native_closure("write", |_thread, _env, args| {
      for arg in args.list {
        print!("{}", arg);
      }

      Value::nil
    });

    let writeln = Value::new_native_closure("writeln", |_thread, _env, args| {
      for arg in args.list {
        print!("{}", arg);
      }
      println!();

      Value::nil
    });

    lib.set("write", write);
    lib.set("writeln", writeln);

    Value::from(lib)
  }
}
