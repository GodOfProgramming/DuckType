use crate::prelude::*;
use std::io::{stdin, stdout, Write};

pub fn console(_: &mut SmartPtr<Gc>, mut lib: UsertypeHandle<ModuleValue>) {
  lib.define("write", Value::native(write));
  lib.define("flush", Value::native(flush));
  lib.define("writeln", Value::native(writeln));
  lib.define("flushln", Value::native(flushln));
  lib.define("readln", Value::native(readln));
}

fn write(_: &mut Vm, args: Args) -> UsageResult {
  for arg in &args.list {
    print!("{}", arg);
  }

  Ok(Value::nil)
}

fn flush(_: &mut Vm, args: Args) -> UsageResult {
  for arg in &args.list {
    print!("{}", arg);
  }

  stdout().flush().ok();

  Ok(Value::nil)
}

fn writeln(_: &mut Vm, args: Args) -> UsageResult {
  for arg in &args.list {
    print!("{}", arg);
  }
  println!();

  Ok(Value::nil)
}

fn flushln(_: &mut Vm, args: Args) -> UsageResult {
  for arg in &args.list {
    print!("{}", arg);
  }
  println!();

  stdout().flush().ok();

  Ok(Value::nil)
}

#[native]
fn readln() -> UsageResult<String> {
  let mut buf = String::new();
  stdin().read_line(&mut buf).map_err(UsageError::native)?;
  Ok(buf.trim().to_string())
}
