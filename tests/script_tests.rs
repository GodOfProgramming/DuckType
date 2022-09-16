use simple_script::{Library, Vm};
use std::{fs, path::Path};
use tfix::{fixture, TestFixture};

struct ScriptTest {
  vm: Vm,
}

impl ScriptTest {
  pub fn run(&mut self, script: &Path) {
    let src = fs::read_to_string(script).unwrap();
    let ctx = self
      .vm
      .load(script.to_string_lossy().to_string(), &src)
      .unwrap();
    self.vm.run(ctx, &mut Default::default()).unwrap();
  }
}

impl TestFixture for ScriptTest {
  fn set_up() -> Self {
    let vm = Vm::new_with_libs(
      &[],
      &[
        Library::Std,
        Library::Env,
        Library::Time,
        Library::String,
        Library::Console,
        Library::Ps,
      ],
    );

    Self { vm }
  }
}

#[fixture(ScriptTest)]
mod tests {
  use super::*;

  #[test]
  fn runs_test_scripts(test: &mut ScriptTest) {
    fs::read_dir("tests/scripts").into_iter().for_each(|dir| {
      dir
        .flatten()
        .into_iter()
        .for_each(|entry| test.run(&entry.path()))
    });
  }
}
