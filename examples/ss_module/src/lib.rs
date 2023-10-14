use simple_script::prelude::*;

fn test_function(_: &mut Vm, _: &mut Env, _: Args) -> ValueResult {
  println!("TESTING");
  Ok(Value::nil)
}

#[no_mangle]
pub fn simple_script_load_module(vm: &mut Vm, env: &mut Env) -> ValueResult<()> {
  env.define(
    "ExampleModule",
    LockedModule::initialize(|lib| {
      lib.set("test_function", Value::native(test_function)).ok();
    })
    .into(),
  );
  Ok(())
}
