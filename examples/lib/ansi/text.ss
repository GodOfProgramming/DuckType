req "common" as common;

export mod text {
  BOLD: "[22m",

  fn enable(code) {
    common::exec(code);
  }

  fn reset() {
    console::write(fmt::reset());
  }

  fn scope(f) {
    f();
    reset();
  }

  color: req "color",

  mod fmt {
    fn reset() {
      ret common::fmt("[0m");
    }
  }
}
