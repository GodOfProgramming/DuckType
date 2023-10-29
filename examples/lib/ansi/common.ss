use std::console;

export mod common {
  fn exec(code) {
    console::write("\x1b" + code);
  }
}
