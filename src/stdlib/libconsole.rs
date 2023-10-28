use crate::prelude::*;

pub struct LibConsole;

pub fn console(gc: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("write", Value::native(print));
  lib.define("writeln", Value::native(println));
  lib.define("flushln", Value::native(flushln));
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
