use simple_script::{Env, Library, NativeClassBuilder, Usertype, UsertypeId, Value, Vm};
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
    let ctx = self.vm.load(script.to_string_lossy().to_string(), &src).unwrap();
    self
      .vm
      .run(
        script.to_string_lossy().to_string(),
        ctx,
        &mut Env::with_library_support(&[], Library::All),
      )
      .unwrap();
  }
}

impl TestFixture for ScriptTest {
  fn set_up() -> Self {
    Self::default()
  }
}

struct Leaker {
  b: &'static mut bool,
  this: Value,
}

impl Drop for Leaker {
  fn drop(&mut self) {
    *self.b = true;
  }
}

impl Usertype for Leaker {
  const ID: UsertypeId = "Leaker";

  fn register(class: &mut NativeClassBuilder<Self>) {
    class.add_setter("this", |this, value| {
      this.this = value;
    });
  }
}

#[fixture(ScriptTest)]
mod tests {
  use simple_script::Value;

  use super::*;

  #[test]
  fn run_test_scripts(t: &mut ScriptTest) {
    fs::read_dir("tests/scripts")
      .into_iter()
      .for_each(|dir| dir.flatten().into_iter().for_each(|entry| t.run(&entry.path())));
  }

  static mut b: bool = false;

  /// This tests for the memory leaking capability of reference counting. This should fail once garbage collection is implemented
  #[test]
  fn memory_leak_test(t: &mut ScriptTest) {
    {
      const SCRIPT: &str = "leaker.this = leaker;";

      let ctx = t.vm.load("test", SCRIPT).unwrap();
      let mut env = Env::with_library_support(&[], Library::None);

      let l = Leaker {
        b: unsafe { &mut b },
        this: Value::nil,
      };
      env.register_usertype::<Leaker>().ok();
      env.define("leaker", Value::from(l));

      t.vm.run("test", ctx, &mut env).unwrap();
    }

    assert!(unsafe { !b });
  }
}
