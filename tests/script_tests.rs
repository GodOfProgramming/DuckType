use macros::{methods, Fields};
use simple_script::prelude::*;
use std::{fs, path::Path};
use tfix::{fixture, TestFixture};

struct ScriptTest {
  vm: Vm,
}

impl ScriptTest {
  pub fn run(&mut self, script: &Path) {
    println!("running {:?}", script);
    let src = fs::read_to_string(script).unwrap();
    let ctx = self.vm.load(script.to_string_lossy().to_string(), &src).unwrap();
    let env = ModuleBuilder::initialize(&mut self.vm.gc, None, |gc, mut lib| {
      lib.env = stdlib::load_libs(gc, lib.handle.value.clone(), &[], &Library::All);
    });
    self.vm.run(script.to_string_lossy().to_string(), ctx, env).unwrap();
  }
}

impl TestFixture for ScriptTest {
  fn set_up() -> Self {
    let mut gc = SmartPtr::new(Gc::default());
    Self {
      vm: Vm::new(gc, vec![], Library::All),
    }
  }
}

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

#[fixture(ScriptTest)]
mod tests {
  use super::*;

  #[test]
  fn run_test_scripts(t: &mut ScriptTest) {
    fs::read_dir("tests/scripts")
      .into_iter()
      .for_each(|dir| dir.flatten().into_iter().for_each(|entry| t.run(&entry.path())));
  }

  /// This tests for the memory leaking capability of reference counting. This should fail once garbage collection is implemented
  #[test]
  fn memory_leak_test(t: &mut ScriptTest) {
    static mut B: bool = false;

    const SCRIPT: &str = "{ let leaker = make_leaker(); leaker.this = leaker; }";

    let ctx = t.vm.load("test", SCRIPT).unwrap();

    #[native]
    fn make_leaker() -> ValueResult<Leaker> {
      Ok(Leaker {
        b: unsafe { &mut B },
        this: Value::nil,
      })
    }

    let mut env = ModuleBuilder::initialize(&mut t.vm.gc, None, |gc, mut lib| {
      lib.env = stdlib::load_libs(gc, lib.handle.value.clone(), &[], &Library::All);
    });

    env.define("make_leaker", Value::native(make_leaker));

    t.vm.run("test", ctx, env).unwrap();

    assert!(unsafe { !B });

    t.vm.run_gc();

    assert!(unsafe { B });
  }
}
