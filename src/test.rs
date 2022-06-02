use super::*;
use tfix::{fixture, TestFixture};

struct IntegrationTest {
  script: String,
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

  fn load<F: FnOnce(&mut Self, SmartPtr<Context>)>(&mut self, f: F) {
    match self.vm.load(String::from("test"), &self.script) {
      Ok(ctx) => f(self, ctx),
      Err(errs) => {
        for err in errs {
          println!("{}", err);
        }
        panic!("compilation errors detected!");
      }
    }
  }

  fn run<F: FnOnce(SmartPtr<Context>, Value)>(&mut self, f: F) {
    self.load(|this, ctx| match this.vm.run(ctx.clone()) {
      Ok(v) => f(ctx, v),
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

  fn adding_a_global(test: &mut IntegrationTest) {
    test.script = "ret foo;".to_string();

    test.load(|this, mut ctx| {
      ctx.assign_global(String::from("foo"), Value::new("foo"));
      match this.vm.run(ctx) {
        Ok(v) => assert_eq!(Value::new("foo"), v),
        Err(err) => panic!("{:#?}", err),
      }
    });
  }

  fn calling_a_native_function(test: &mut IntegrationTest) {
    test.script = "let x = 1;\n
    ret test_func(x, 2);"
      .to_string();

    test.load(|this, mut ctx| {
      ctx.create_native(String::from("test_func"), |args: Vec<Value>| {
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], Value::new(1f64));
        assert_eq!(args[1], Value::new(2f64));
        Ok(Value::new(3f64))
      });
      match this.vm.run(ctx) {
        Ok(v) => assert_eq!(Value::new(3f64), v),
        Err(err) => panic!("{:#?}", err),
      }
    });
  }

  /**
   * nil
   */
  fn let_0(test: &mut IntegrationTest) {
    test.script = "let foo;".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::Nil);
    });
  }

  /**
   * true
   */
  fn let_1(test: &mut IntegrationTest) {
    test.script = "let foo = true;".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(true));
    });
  }

  /**
   * add sub mul div mod
   */
  fn let_2(test: &mut IntegrationTest) {
    test.script = "let foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(7.2));
    });
  }

  /**
   * add sub mul div mod
   */
  fn let_3(test: &mut IntegrationTest) {
    test.script = "let foo; foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(7.2));
    });
  }

  fn block_0(test: &mut IntegrationTest) {
    test.script = "let foo; { foo = 1; }".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
    });
  }

  fn block_1(test: &mut IntegrationTest) {
    test.script = "let foo; { foo = 1; let bar; bar = 0; }".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
      assert!(ctx.lookup_global("bar").is_none());
    });
  }

  fn if_0(test: &mut IntegrationTest) {
    test.script = "let foo = true; if foo { foo = 1; }".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
    });
  }

  /**
   * else block not executed
   */
  fn if_1(test: &mut IntegrationTest) {
    test.script = "let foo = true; if foo { foo = 1; } else { foo = 2; }".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
    });
  }

  fn if_2(test: &mut IntegrationTest) {
    test.script = "let foo = false; if foo { foo = 1; } else { foo = 2; }".to_string();

    test.run(|ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(2));
    });
  }

  fn if_3(test: &mut IntegrationTest) {
    test.script = "if true and false or true { ret true; } else { ret false; }".to_string();

    test.run(|_, v| assert!(v.truthy()));
  }

  fn if_4(test: &mut IntegrationTest) {
    test.script =
      "if true and false and false or true { ret true; } else { ret false; }".to_string();

    test.run(|_, v| assert!(v.truthy()));
  }

  fn if_5(test: &mut IntegrationTest) {
    test.script =
      "if true and false and (false or true) { ret true; } else { ret false; }".to_string();

    test.run(|_, v| assert!(!v.truthy()));
  }
}
