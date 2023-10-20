use macros::{methods, Fields};
use simple_script::prelude::*;
use std::{fs, path::Path};
use tfix::{fixture, TestFixture};

#[derive(Default)]
struct ScriptTest {
  vm: Vm,
}

impl ScriptTest {
  pub fn run(&mut self, script: &Path) {
    println!("running {:?}", script);
    let src = fs::read_to_string(script).unwrap();
    let env = Env::initialize(&mut self.vm.gc, &[], Library::All);
    let ctx = self.vm.load(script.to_string_lossy().to_string(), &src, env).unwrap();
    self.vm.run(script.to_string_lossy().to_string(), ctx).unwrap();
  }
}

impl TestFixture for ScriptTest {
  fn set_up() -> Self {
    Self {
      vm: Vm::new(vec![], Library::All),
    }
  }
}

#[derive(Usertype, Fields)]
#[uuid("8d77f7e3-ccad-4214-a7d1-98f283b7a624")]
struct Leaker {
  b: &'static mut bool,

  #[field]
  this: Value,
}

#[methods]
impl Leaker {}

impl Drop for Leaker {
  fn drop(&mut self) {
    *self.b = true;
  }
}

impl TraceableValue for Leaker {
  fn trace(&self, marks: &mut Marker) {
    marks.trace(&self.this);
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

  static mut B: bool = false;

  /// This tests for the memory leaking capability of reference counting. This should fail once garbage collection is implemented
  #[test]
  fn memory_leak_test(t: &mut ScriptTest) {
    {
      const SCRIPT: &str = "print(leaker); print(leaker.this); leaker.this = leaker; print(leaker.this);";

      let env = Env::initialize(&mut t.vm.gc, &[], Library::All);
      let mut ctx = t.vm.load("test", SCRIPT, env).unwrap();

      let l = Leaker {
        b: unsafe { &mut B },
        this: Value::nil,
      };

      ctx.env.define("leaker", t.vm.gc.allocate(l));

      t.vm.run("test", ctx).unwrap();
    }

    assert!(unsafe { !B });
  }
}
