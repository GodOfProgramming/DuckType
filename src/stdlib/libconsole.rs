use crate::{Args, ComplexValue, Env, ExecutionThread, StructValue, Value};

pub struct LibConsole;

impl LibConsole {
  pub fn load() -> Value {
    let mut lib = StructValue::default();

    lib.set("write", Value::new_native_fn(Self::print)).ok();
    lib.set("writeln", Value::new_native_fn(Self::println)).ok();
    lib.set("flushln", Value::new_native_fn(Self::flushln)).ok();

    Value::from(lib)
  }

  fn print(_thread: &mut ExecutionThread, _env: &mut Env, args: Args) -> Value {
    for arg in args.list {
      print!("{}", arg);
    }

    Value::nil
  }

  fn println(_thread: &mut ExecutionThread, _env: &mut Env, args: Args) -> Value {
    for arg in args.list {
      print!("{}", arg);
    }
    println!();

    Value::nil
  }

  fn flushln(_thread: &mut ExecutionThread, _env: &mut Env, args: Args) -> Value {
    use std::io::{stdout, Write};
    for arg in args.list {
      print!("{}", arg);
    }
    println!();
    stdout().flush().ok();

    Value::nil
  }
}
