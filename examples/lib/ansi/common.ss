use std::console;

export mod {
  fn exec(code) {
    console::write("\x1b" + code);
  }
}
