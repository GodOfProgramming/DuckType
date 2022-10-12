use simple_script::{ComplexValue, ComplexValueId, Env, Library, SetResult, StructValue, Value, Vm};
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

struct Leaker<'b> {
  b: &'b mut bool,
  data: StructValue,
}

impl<'b> Drop for Leaker<'b> {
  fn drop(&mut self) {
    *self.b = true;
  }
}

impl<'b> ComplexValue for Leaker<'b> {
  const ID: ComplexValueId = "Leaker";

  fn set(&mut self, name: &str, value: Value) -> SetResult {
    self.data.set(name, value)
  }

  fn get(&self, name: &str) -> Value {
    self.data.get(name)
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

  /// This tests for the memory leaking capability of reference counting. This should fail once garbage collection is implemented
  #[test]
  fn memory_leak_test(t: &mut ScriptTest) {
    let mut b = false;

    {
      const SCRIPT: &str = "leaker.this = leaker;";

      let ctx = t.vm.load("test", SCRIPT).unwrap();
      let mut env = Env::with_library_support(&[], Library::None);

      let l = Leaker {
        b: &mut b,
        data: Default::default(),
      };

      env.define("leaker", Value::from(l));

      t.vm.run("test", ctx, &mut env).unwrap();
    }

    assert!(!b);
  }
}
