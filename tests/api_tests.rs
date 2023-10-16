use simple_script::prelude::*;
use tfix::prelude::*;

const TEST_FILE: &str = "test";

#[derive(Default)]
struct ApiTest {
  vm: Vm,
}

impl TestFixture for ApiTest {
  fn set_up() -> Self {
    Self::default()
  }
}

#[fixture(ApiTest)]
mod tests {
  use super::*;

  #[test]
  fn can_register_global_variables(t: &mut ApiTest) {
    let script = "export some_var;";
    let env = Env::initialize(&[], Library::All);
    let mut ctx = t.vm.load(TEST_FILE, script, env).unwrap();
    ctx.env.define("some_var", Value::from(true));
    if let Return::Value(v) = t.vm.run(TEST_FILE, ctx).unwrap() {
      assert!(v == Value::from(true));
    } else {
      panic!();
    }
  }

  #[test]
  fn can_register_lambda(t: &mut ApiTest) {
    let script = "export some_func();";
    let env = Env::initialize(&[], Library::All);
    let mut ctx = t.vm.load("test", script, env).unwrap();
    ctx.env.define("some_func", Value::native(|_, _args| Ok(Value::from(true))));

    if let Return::Value(v) = t.vm.run(TEST_FILE, ctx).unwrap() {
      assert!(v == Value::from(true));
    } else {
      panic!();
    }
  }

  #[test]
  fn can_yield(t: &mut ApiTest) {
    let script = "let x = true; yield; export x;";
    let env = Env::initialize(&[], Library::All);
    let ctx = t.vm.load("test", script, env).unwrap();
    let result = t.vm.run(TEST_FILE, ctx).unwrap();

    if let Return::Yield(y) = result {
      if let Return::Value(v) = t.vm.resume(y).unwrap() {
        assert!(v == Value::from(true));
      } else {
        panic!();
      }
    } else {
      panic!("failed to yield in script");
    }
  }
}
