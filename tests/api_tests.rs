use ducktype::prelude::*;
use tfix::prelude::*;

struct ApiTest {
  vm: Vm,
  env: UsertypeHandle<ModuleValue>,
}

impl TestFixture for ApiTest {
  fn set_up() -> Self {
    let mut gc = SmartPtr::new(Gc::default());
    let env = ModuleBuilder::initialize(&mut gc, "*test*", None, |gc, mut lib| {
      lib.env = stdlib::enable_std(gc, lib.value(), &[]);
    });

    Self {
      vm: Vm::new(gc, []),
      env,
    }
  }
}

#[fixture(ApiTest)]
mod tests {
  use super::*;

  #[test]
  fn can_register_global_variables(t: &mut ApiTest) {
    let script = "export some_var;";
    assert!(t.env.define("some_var", Value::from(true)));
    let res = t.vm.run_string(script, t.env.clone()).unwrap();
    assert!(res == Value::from(true));
  }

  #[test]
  fn can_register_lambda(t: &mut ApiTest) {
    let script = "export some_func();";
    assert!(t.env.define("some_func", Value::native(|_, _args| Ok(Value::from(true)))));
    let res = t.vm.run_string(script, t.env.clone()).unwrap();
    assert!(res == Value::from(true));
  }
}
