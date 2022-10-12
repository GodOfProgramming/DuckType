use super::*;
use tfix::{fixture, TestFixture};

const TEST_FILE: &str = "test";

struct IntegrationTest {
  script: &'static str,
  vm: Vm,
}

impl IntegrationTest {
  fn new() -> Self {
    let vm = Vm::default();

    Self {
      script: Default::default(),
      vm,
    }
  }

  fn load<F: FnOnce(&mut Self, SmartPtr<Context>, &mut Env)>(&mut self, f: F) {
    match self.vm.load(TEST_FILE, &self.script) {
      Ok(ctx) => {
        let mut env = Env::default();
        f(self, ctx, &mut env);
      }
      Err(errs) => {
        for err in errs {
          println!("{}", err);
        }
        panic!("compilation errors detected!");
      }
    }
  }

  fn run<F: FnOnce(SmartPtr<Context>, &Env, Value)>(&mut self, f: F) {
    self.load(|this, ctx, env| match this.vm.run(TEST_FILE, ctx.clone(), env) {
      Ok(v) => match v {
        RunResult::Value(v) => f(ctx, env, v),
        RunResult::Yield(_) => panic!("this test function should not be used for yields"),
      },
      Err(err) => panic!("{:#?}", err),
    });
  }
}

impl TestFixture for IntegrationTest {
  fn set_up() -> Self {
    Self::new()
  }
}

#[fixture(IntegrationTest)]
mod integration_tests {
  use super::*;

  #[test]
  fn adding_a_global(test: &mut IntegrationTest) {
    test.script = "ret foo;";

    test.load(|this, ctx, env| {
      env.assign(String::from("foo"), Value::from("foo"));
      match this.vm.run(TEST_FILE, ctx, env) {
        Ok(res) => match res {
          RunResult::Value(v) => {
            assert_eq!(Value::from("foo"), v);
          }
          RunResult::Yield(_) => panic!("should not use yields"),
        },
        Err(err) => panic!("{:#?}", err),
      }
    });
  }

  #[test]
  fn calling_a_native_function(test: &mut IntegrationTest) {
    test.script = "let x = 1; ret test_func(x, 2);";

    test.load(|this, ctx, env| {
      env.define(
        "test_func",
        Value::new_native_fn(|_thread, _env, args| {
          let args = args.list;
          assert_eq!(args.len(), 2);
          assert_eq!(args[0], Value::from(1));
          assert_eq!(args[1], Value::from(2));
          Value::from(3f64)
        }),
      );
      match this.vm.run("test", ctx, env) {
        Ok(res) => match res {
          RunResult::Value(v) => assert_eq!(Value::from(3f64), v),
          RunResult::Yield(_) => panic!("should not yield"),
        },
        Err(err) => panic!("{:#?}", err),
      }
    });
  }

  /**
   * nil
   */
  #[test]
  fn let_0(test: &mut IntegrationTest) {
    test.script = "let $foo;";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::nil);
    });
  }

  /**
   * true
   */
  #[test]
  fn let_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true;";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(true));
    });
  }

  #[test]
  fn let_2(test: &mut IntegrationTest) {
    test.script = "let $foo = 1 + 2 * 3 - 4.0 / 5 + 6 % 5;";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(7.2));
    });
  }

  #[test]
  fn let_3(test: &mut IntegrationTest) {
    test.script = "let $foo; $foo = 1 + 2 * 3 - 4.0 / 5 + 6 % 5;";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(7.2));
    });
  }

  #[test]
  fn block_0(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; }";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn block_1(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; let bar; bar = 0; }";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
      assert!(env.lookup("bar").is_none());
    });
  }

  #[test]
  fn if_0(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; }";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  /**
   * else block not executed
   */
  #[test]
  fn if_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; } else { $foo = 2; }";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn if_2(test: &mut IntegrationTest) {
    test.script = "let $foo = false; if $foo { $foo = 1; } else { $foo = 2; }";

    test.run(|_ctx, env, _| {
      let val = env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(2));
    });
  }

  #[test]
  fn if_3(test: &mut IntegrationTest) {
    test.script = "if true and false or true { ret true; } else { ret false; }";

    test.run(|_, _, v| assert!(v.truthy()));
  }

  #[test]
  fn if_4(test: &mut IntegrationTest) {
    test.script = "if true and false and false or true { ret true; } else { ret false; }";

    test.run(|_, _, v| assert!(v.truthy()));
  }

  #[test]
  fn if_5(test: &mut IntegrationTest) {
    test.script = "if true and false and (false or true) { ret true; } else { ret false; }";

    test.run(|_, _, v| {
      println!("{:?}", v);
      assert!(v.falsy());
    });
  }
}
