use crate::prelude::*;

pub struct LibEnv;

impl LibEnv {
  pub fn load(gc: &mut Gc, args: &[String]) -> Value {
    LockedModule::initialize(gc, |gc, lib| {
      let args = args.iter().map(|arg| gc.allocate(arg.clone())).collect::<Vec<Value>>();
      let args = gc.allocate(args);
      lib.set(gc, "ARGV", args).ok();
    })
  }
}
