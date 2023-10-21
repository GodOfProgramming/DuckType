use crate::prelude::*;

pub struct LibConsole;

impl LibConsole {
  pub fn load(gc: &mut Gc) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      lib.set(gc, "write", Value::native(Self::print)).ok();
      lib.set(gc, "writeln", Value::native(Self::println)).ok();
      lib.set(gc, "flushln", Value::native(Self::flushln)).ok();
    })
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
