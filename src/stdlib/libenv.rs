use crate::prelude::*;

pub struct LibEnv;

impl LibEnv {
  pub fn load(gc: &mut SmartPtr<Gc>, gmod: Value, args: &[String]) -> UsertypeHandle<ModuleValue> {
    ModuleBuilder::initialize(gc, Some(gmod), |gc, mut lib| {
      let args = args.iter().map(|arg| gc.allocate(arg.clone())).collect::<Vec<Value>>();
      let args = gc.allocate(args);
      lib.define("ARGV", args);
    })
  }
}
