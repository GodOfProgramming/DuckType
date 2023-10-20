use crate::prelude::*;
use ptr::SmartPtr;
use tfix::{fixture, TestFixture};

const TEST_FILE: &'static str = "test";

struct IntegrationTest {
  script: String,
  vm: Vm,
}

impl IntegrationTest {
  fn new() -> Self {
    let vm = Vm::new([], Default::default());

    Self {
      script: Default::default(),
      vm,
    }
  }

  fn load<F: FnOnce(&mut Self, SmartPtr<Context>)>(&mut self, f: F) {
    let env = Env::initialize(&mut self.vm.gc, &[], Library::All);
    match self.vm.load(TEST_FILE, &self.script, env) {
      Ok(ctx) => {
        f(self, ctx);
      }
      Err(errs) => {
        for err in errs {
          println!("{}", err);
        }
        panic!("compilation errors detected!");
      }
    }
  }

  fn run<F: FnOnce(SmartPtr<Context>, Value)>(&mut self, f: F) {
    self.load(|this, ctx| match this.vm.run(TEST_FILE, ctx.clone()) {
      Ok(v) => match v {
        Return::Value(v) => f(ctx, v),
        Return::Yield(_) => panic!("this test function should not be used for yields"),
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
  use crate::memory::Allocation;

  use super::*;
  use evalexpr::eval;

  #[test]
  fn adding_a_global(test: &mut IntegrationTest) {
    test.script = "export foo;".into();

    test.load(|this, mut ctx| {
      ctx.env.assign(String::from("foo"), this.vm.gc.allocate("foo"));
      match this.vm.run(TEST_FILE, ctx) {
        Ok(res) => match res {
          Return::Value(v) => {
            if let Some(v) = v.as_str() {
              assert_eq!("foo", **v);
            } else {
              panic!("value is not a string");
            }
          }
          Return::Yield(_) => panic!("should not use yields"),
        },
        Err(err) => panic!("{:#?}", err),
      }
    });
  }

  #[test]
  fn calling_a_native_function(test: &mut IntegrationTest) {
    test.script = "let x = 1; export test_func(x, 2);".into();

    test.load(|this, mut ctx| {
      ctx.env.define(
        "test_func",
        Value::native(|_, args| {
          let args = &args.list;
          assert_eq!(args.len(), 2);
          assert_eq!(args[0], Value::from(1));
          assert_eq!(args[1], Value::from(2));
          Ok(Value::from(3f64))
        }),
      );
      match this.vm.run("test", ctx) {
        Ok(res) => match res {
          Return::Value(v) => assert_eq!(Value::from(3f64), v),
          Return::Yield(_) => panic!("should not yield"),
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
    test.script = "let $foo;".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::nil);
    });
  }

  /**
   * true
   */
  #[test]
  fn let_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true;".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(true));
    });
  }

  #[test]
  fn let_2(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo = {math};");

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn let_3(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo; $foo = {math};");

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn block_0(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; }".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn block_1(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; let bar; bar = 0; }".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
      assert!(ctx.env.lookup("bar").is_none());
    });
  }

  #[test]
  fn if_0(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; }".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  /**
   * else block not executed
   */
  #[test]
  fn if_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; } else { $foo = 2; }".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn if_2(test: &mut IntegrationTest) {
    test.script = "let $foo = false; if $foo { $foo = 1; } else { $foo = 2; }".into();

    test.run(|ctx, _| {
      let val = ctx.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(2));
    });
  }

  #[test]
  fn if_3(test: &mut IntegrationTest) {
    test.script = "let x = false; if true and false or true { x = true; } else { x = false; } export x;".into();

    test.run(|_, v| assert!(v.truthy()));
  }

  #[test]
  fn if_4(test: &mut IntegrationTest) {
    test.script = "let x = false; if true and false and false or true { x = true; } else { x = false; } export x;".into();

    test.run(|_, v| assert!(v.truthy()));
  }

  #[test]
  fn if_5(test: &mut IntegrationTest) {
    test.script = "let x = true; if true and false and (false or true) { x = true; } else { x = false; } export x;".into();

    test.run(|_, v| {
      println!("{:?}", v);
      assert!(v.falsy());
    });
  }
}
