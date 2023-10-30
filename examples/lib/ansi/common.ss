use std::console;
use std::str;

export mod common {
  fn exec(code) {
    console::write(fmt(code));
  }

  fn fmt(code) {
    ret str::concat("\x1b", code);
  }
}
