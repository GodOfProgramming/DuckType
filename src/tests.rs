use crate::{prelude::*, value::prelude::module_value::ModuleType};
use ptr::SmartPtr;
use std::path::Path;
use tfix::{fixture, TestFixture};

struct IntegrationTest {
  script: String,
  vm: Vm,
  libstd: UsertypeHandle<ModuleValue>,
}

impl IntegrationTest {
  fn new() -> Self {
    let gc = SmartPtr::new(Gc::test_default());
    let mut vm = Vm::new(gc, false, []);
    let libstd = vm.generate_stdlib("*test*");

    Self {
      script: Default::default(),
      vm,
      libstd,
    }
  }

  fn run<F: FnOnce(&mut Self, Value)>(&mut self, f: F) {
    let env = self.libstd.clone();
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

    test.libstd.define(String::from("foo"), test.vm.make_value_from("foo"));
    match test.vm.run_string(&test.script, test.libstd.clone()) {
      Ok(res) => {
        assert_eq!("foo", **res.cast_to::<StringValue>().expect("value is not a string"));
      }

      Err(err) => panic!("{:#?}", err),
    };
  }

  #[test]
  fn calling_a_native_function(test: &mut IntegrationTest) {
    test.script = "let x = 1; export test_func(x, 2);".into();

    test.libstd.define(
      "test_func",
      Value::new::<NativeFn>(|_, args| {
        let args = &args.list;
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], Value::from(1));
        assert_eq!(args[1], Value::from(2));
        Ok(Value::from(3f64))
      }),
    );

    match test.vm.run_string(&test.script, test.libstd.clone()) {
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
      let val = this.vm.get_global("$foo").unwrap();
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
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(true));
    });
  }

  #[test]
  fn let_2(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo = {math};");

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn let_3(test: &mut IntegrationTest) {
    let math = "1 + 2 * 3 - 4.0 / 5 + 6 % 5";
    let value = eval(math).unwrap().as_float().unwrap();
    test.script = format!("let $foo; $foo = {math};");

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(value));
    });
  }

  #[test]
  fn block_0(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; }".into();

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn block_1(test: &mut IntegrationTest) {
    test.script = "let $foo; { $foo = 1; let bar; bar = 0; }".into();

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(1));
      assert!(this.vm.get_global("bar").is_none());
    });
  }

  #[test]
  fn if_0(test: &mut IntegrationTest) {
    test.script = "let $foo = true; if $foo { $foo = 1; }".into();

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
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
      let val = this.vm.get_global("$foo").unwrap();
      assert_eq!(val, Value::from(1));
    });
  }

  #[test]
  fn if_2(test: &mut IntegrationTest) {
    test.script = "let $foo = false; if $foo { $foo = 1; } else { $foo = 2; }".into();

    test.run(|this, _| {
      let val = this.vm.get_global("$foo").unwrap();
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

struct ScriptTest {
  vm: Vm,
  opt: Vm,
}

impl ScriptTest {
  pub fn run(&mut self, script: &Path) {
    // non-opt
    {
      println!("Running non-optimized {:?}", script);
      let env = ModuleBuilder::initialize(&mut self.vm, ModuleType::new_global("*test*"), |gc, mut lib| {
        let libval = lib.value();
        lib.env.extend(stdlib::make_stdlib(gc, libval, &[]));
      });
      self.vm.run_file(script, env).unwrap();
    }

    // opt
    {
      println!("Running optimized {:?}", script);
      let env = ModuleBuilder::initialize(&mut self.opt, ModuleType::new_global("*test*"), |gc, mut lib| {
        let libval = lib.value();
        lib.env.extend(stdlib::make_stdlib(gc, libval, &[]));
      });
      self.opt.run_file(script, env).unwrap();
    }
  }
}

impl TestFixture for ScriptTest {
  fn set_up() -> Self {
    Self {
      vm: Vm::new(SmartPtr::new(Gc::test_default()), false, []),
      opt: Vm::new(SmartPtr::new(Gc::test_default()), true, []),
    }
  }
}

#[fixture(ScriptTest)]
mod script_tests {
  use super::*;
  use itertools::Itertools;
  use std::fs;

  #[test]
  fn run_test_scripts(t: &mut ScriptTest) {
    #[cfg(feature = "check-opcodes")]
    use {
      crate::code::gen::{CAPTURE_OPS, GENERATED_OPS},
      strum::IntoEnumIterator,
    };

    #[cfg(feature = "check-opcodes")]
    {
      CAPTURE_OPS.set(true);
    }

    fs::read_dir("tests/scripts").into_iter().for_each(|dir| {
      dir
        .flatten()
        .into_iter()
        .sorted_by_cached_key(|entry| entry.path())
        .for_each(|entry| t.run(&entry.path()))
    });

    #[cfg(feature = "check-opcodes")]
    {
      GENERATED_OPS.with_borrow(|ops| {
        let mut unaccounted = Vec::new();
        for op in Opcode::iter() {
          if !matches!(op, Opcode::Unknown | Opcode::Breakpoint) {
            if !ops.contains(&op) {
              unaccounted.push(op);
            }
          }
        }
        if !unaccounted.is_empty() {
          panic!("Did not account for opcodes in tests: {}", itertools::join(unaccounted, ", "));
        }
      });

      CAPTURE_OPS.set(false);
    }
  }
}
