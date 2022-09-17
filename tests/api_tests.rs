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
    let script = "ret some_var;";
    let ctx = t.vm.load(TEST_FILE, script).unwrap();
    let mut env = Env::with_library_support(&[], Library::All);
    env.define("some_var", Value::new(true));
    assert!(matches!(
      t.vm.run(TEST_FILE, ctx, &mut env).unwrap(),
      RunResult::Value(Value::Bool(true))
    ));
  }

  #[test]
  fn can_register_lambda(t: &mut ApiTest) {
    let script = "ret some_func();";
    let ctx = t.vm.load("test", script).unwrap();
    let mut env = Env::with_library_support(&[], Library::All);
    env.create_native("some_func", |_thread, _env, _args| Ok(Value::new(true)));
    assert!(matches!(
      t.vm.run(TEST_FILE, ctx, &mut env).unwrap(),
      RunResult::Value(Value::Bool(true))
    ));
  }

  #[test]
  fn can_yield(t: &mut ApiTest) {
    let script = "let x = true; yield; ret x;";
    let ctx = t.vm.load("test", script).unwrap();
    let mut env = Env::with_library_support(&[], Library::All);
    let result = t.vm.run(TEST_FILE, ctx, &mut env).unwrap();

    if let RunResult::Yield(y) = result {
      assert!(matches!(
        t.vm.resume(y, &mut env).unwrap(),
        RunResult::Value(Value::Bool(true))
      ));
    } else {
      panic!("failed to yield in script");
    }
  }
}
