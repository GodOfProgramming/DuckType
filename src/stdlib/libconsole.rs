use crate::prelude::*;

pub fn console(_: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("write", Value::native(write));
  lib.define("writeln", Value::native(writeln));
  lib.define("flushln", Value::native(flushln));
}

fn write(_: &mut Vm, args: Args) -> ValueResult {
  for arg in &args.list {
    print!("{}", arg);
  }

  Ok(Value::nil)
}

fn writeln(_: &mut Vm, args: Args) -> ValueResult {
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
