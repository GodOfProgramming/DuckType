use crate::prelude::*;

pub struct LibConsole;

impl LibConsole {
  pub fn load(gc: &mut SmartPtr<Gc>, gmod: Value) -> UsertypeHandle<ModuleValue> {
    ModuleBuilder::initialize(gc, Some(gmod), |_, mut lib| {
      lib.define("write", Value::native(Self::print));
      lib.define("writeln", Value::native(Self::println));
      lib.define("flushln", Value::native(Self::flushln));
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
