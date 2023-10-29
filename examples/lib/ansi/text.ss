req "common" as common;
req "color" as color;

export mod text {
  BOLD: "[22m",

  color,

  fn enable(code) {
    common::exec(code);
  }

  fn reset() {
    common::exec("[0m");
  }

  fn scope(f) {
    f();
    reset();
  }
}
