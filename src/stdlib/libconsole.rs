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

  fn print(_: &mut Vm, args: Args) -> ValueResult {
    for arg in &args.list {
      print!("{}", arg);
    }

    Ok(Value::nil)
  }

  fn println(_: &mut Vm, args: Args) -> ValueResult {
    for arg in &args.list {
      print!("{}", arg);
    }
    println!();

    Ok(Value::nil)
  }

  fn flushln(_: &mut Vm, args: Args) -> ValueResult {
    use std::io::{stdout, Write};
    for arg in &args.list {
      print!("{}", arg);
    }
    println!();
    stdout().flush().ok();

    Ok(Value::nil)
  }
}
