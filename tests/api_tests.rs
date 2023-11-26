use ducktype::prelude::*;
use macros::Fields;
use tfix::prelude::*;

struct ApiTest {
  vm: Vm,
  stdlib: UsertypeHandle<ModuleValue>,
}

impl TestFixture for ApiTest {
  fn set_up() -> Self {
    let gc = SmartPtr::new(Gc::new(Memory::Mb(100)));
    let mut vm = Vm::new(gc, false, []);
    let stdlib = vm.generate_stdlib("*test*");

    Self { vm, stdlib }
  }
}

#[fixture(ApiTest)]
mod tests {
  use super::*;

  #[derive(Usertype, Fields, NoOperators)]
  #[uuid("8d77f7e3-ccad-4214-a7d1-98f283b7a624")]
  struct Leaker {
    b: &'static mut bool,

    #[trace]
    this: Value,
  }

  #[methods]
  impl Leaker {
    fn set_this(&mut self, other: Value) -> UsageResult<()> {
      self.this = other;
      Ok(())
    }
  }

  impl Drop for Leaker {
    fn drop(&mut self) {
      *self.b = true;
    }
  }

  #[test]
  fn can_register_global_variables(t: &mut ApiTest) {
    let script = "export some_var;";
    assert!(t.stdlib.define("some_var", Value::from(true)));
    let res = t.vm.run_string(script, t.stdlib.clone()).unwrap();
    assert!(res == Value::from(true));
  }

  #[test]
  fn can_register_lambda(t: &mut ApiTest) {
    let script = "export some_func();";
    assert!(t
      .stdlib
      .define("some_func", Value::new::<NativeFn>(|_, _args| Ok(Value::from(true)))));
    let res = t.vm.run_string(script, t.stdlib.clone()).unwrap();
    assert!(res == Value::from(true));
  }

  #[test]
  fn memory_leak_test(t: &mut ApiTest) {
    static mut B: bool = false;

    const SCRIPT: &str = "{ let leaker = make_leaker(); leaker.set_this(leaker); }";

    #[native]
    fn make_leaker() -> UsageResult<Leaker> {
      Ok(Leaker {
        b: unsafe { &mut B },
        this: Value::nil,
      })
    }

    let mut stdlib = t.vm.generate_stdlib("*test*");

    stdlib.define("make_leaker", Value::new::<NativeFn>(make_leaker));

    t.vm.run_string(SCRIPT, stdlib).unwrap();

    assert!(unsafe { !B });

    t.vm.force_gc();

    assert!(unsafe { B });
  }
}
