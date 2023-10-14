use crate::prelude::*;

pub struct LibConsole;

impl LibConsole {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      lib.set("write", Value::native(Self::print)).ok();
      lib.set("writeln", Value::native(Self::println)).ok();
      lib.set("flushln", Value::native(Self::flushln)).ok();
    })
    .into()
  }

  fn print(_vm: &mut Vm, _env: &mut Env, args: Args) -> ValueResult {
    for arg in args.list {
      print!("{}", arg);
    }

    Ok(Value::nil)
  }

  fn println(_vm: &mut Vm, _env: &mut Env, args: Args) -> ValueResult {
    for arg in args.list {
      print!("{}", arg);
    }
    println!();

    Ok(Value::nil)
  }

  fn flushln(_vm: &mut Vm, _env: &mut Env, args: Args) -> ValueResult {
    use std::io::{stdout, Write};
    for arg in args.list {
      print!("{}", arg);
    }
    println!();
    stdout().flush().ok();

    Ok(Value::nil)
  }
}
