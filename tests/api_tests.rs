use std::time::Duration;

use ducktype::prelude::*;
use macros::{methods, Fields};
use tfix::prelude::*;

struct ApiTest {
  vm: Vm,
  env: UsertypeHandle<ModuleValue>,
}

impl TestFixture for ApiTest {
  fn set_up() -> Self {
    let mut gc = SmartPtr::new(Gc::new(Duration::from_nanos(0)));
    let env = ModuleBuilder::initialize(&mut gc, ModuleType::new_global("*test*"), |gc, mut lib| {
      let libval = lib.handle.value.clone();
      lib.env.extend(stdlib::enable_std(gc, libval, &[]));
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

  #[derive(Usertype, Fields)]
  #[uuid("8d77f7e3-ccad-4214-a7d1-98f283b7a624")]
  struct Leaker {
    b: &'static mut bool,

    #[field]
    #[trace]
    this: Value,
  }

  #[methods]
  impl Leaker {}

  impl Drop for Leaker {
    fn drop(&mut self) {
      *self.b = true;
    }
  }

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

  #[test]
  fn memory_leak_test(t: &mut ApiTest) {
    static mut B: bool = false;

    const SCRIPT: &str = "{ let leaker = make_leaker(); leaker.this = leaker; }";

    #[native]
    fn make_leaker() -> UsageResult<Leaker> {
      Ok(Leaker {
        b: unsafe { &mut B },
        this: Value::nil,
      })
    }

    let mut env = ModuleBuilder::initialize(&mut t.vm.gc, ModuleType::new_global("*test*"), |gc, mut lib| {
      let libval = lib.handle.value.clone();
      lib.env.extend(stdlib::enable_std(gc, libval, &[]));
    });

    env.define("make_leaker", Value::native(make_leaker));

    t.vm.run_string(SCRIPT, env).unwrap();

    assert!(unsafe { !B });

    t.vm.run_gc(None).unwrap();

    t.vm.gc.terminate();

    assert!(unsafe { B });
  }
}
