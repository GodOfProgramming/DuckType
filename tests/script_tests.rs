use simple_script::{Env, Library, Vm};
use std::{fs, path::Path};
use tfix::{fixture, TestFixture};

#[derive(Default)]
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

#[fixture(ScriptTest)]
mod tests {
  use super::*;

  #[test]
  fn run_test_scripts(test: &mut ScriptTest) {
    fs::read_dir("tests/scripts").into_iter().for_each(|dir| {
      dir
        .flatten()
        .into_iter()
        .for_each(|entry| test.run(&entry.path()))
    });
  }
}
