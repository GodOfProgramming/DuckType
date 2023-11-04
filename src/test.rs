use crate::prelude::*;
use ptr::SmartPtr;
use tfix::{fixture, TestFixture};

struct IntegrationTest {
  script: String,
  vm: Vm,
  env: UsertypeHandle<ModuleValue>,
}

impl IntegrationTest {
  fn new() -> Self {
    let mut gc = SmartPtr::new(Gc::default());
    let env = ModuleBuilder::initialize(&mut gc, "*test*", None, |gc, mut lib| {
      lib.env = stdlib::enable_std(gc, lib.handle.value.clone(), &[]);
    });

    let vm = Vm::new(gc, []);

    Self {
      script: Default::default(),
      vm,
      env,
    }
  }

  fn run<F: FnOnce(&mut Self, Value)>(&mut self, f: F) {
    let env = self.env.clone();
    match self.vm.run_string(&self.script, env) {
      Ok(v) => f(self, v),
      Err(err) => panic!("{}", err),
    }
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
  use evalexpr::eval;

  #[test]
  fn adding_a_global(test: &mut IntegrationTest) {
    test.script = "export foo;".into();

    test.env.define(String::from("foo"), test.vm.gc.allocate("foo"));
    match test.vm.run_string(&test.script, test.env.clone()) {
      Ok(res) => {
        assert_eq!("foo", **res.as_str().expect("value is not a string"));
      }

      Err(err) => panic!("{:#?}", err),
    };
  }

  #[test]
  fn calling_a_native_function(test: &mut IntegrationTest) {
    test.script = "let x = 1; export test_func(x, 2);".into();

    test.env.define(
      "test_func",
      Value::native(|_, args| {
        let args = &args.list;
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], Value::from(1));
        assert_eq!(args[1], Value::from(2));
        Ok(Value::from(3f64))
      }),
    );

    match test.vm.run_string(&test.script, test.env.clone()) {
      Ok(res) => assert_eq!(Value::from(3f64), res),
      Err(err) => panic!("{:#?}", err),
    };
  }

  /**
   * nil
   */
  #[test]
  fn let_0(test: &mut IntegrationTest) {
    test.script = "let $foo;".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::nil);
    });
  }

  /**
   * true
   */
  #[test]
  fn let_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true;".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(true));
    });
  }

  #[test]
  fn let_2(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo = {math};");

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn let_3(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo; $foo = {math};");

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn block_0(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; }".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn block_1(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; let bar; bar = 0; }".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
      assert!(this.env.lookup("bar").is_none());
    });
  }

  #[test]
  fn if_0(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; }".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  /**
   * else block not executed
   */
  #[test]
  fn if_1(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; } else { $foo = 2; }".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn if_2(test: &mut IntegrationTest) {
    test.script = "let $foo = false; if $foo { $foo = 1; } else { $foo = 2; }".into();

    test.run(|this, _| {
      let val = this.env.lookup("$foo").unwrap();
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
