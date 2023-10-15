use simple_script::prelude::*;

#[no_mangle]
pub fn simple_script_load_module(vm: &mut Vm, env: &mut Env) -> ValueResult<()> {
  example_module::register_to(env);
  Ok(())
}

#[native]
mod example_module {
  fn test_function(item: &StringValue) -> ValueResult<()> {
    println!("{}", item);
    Ok(())
  }
}
