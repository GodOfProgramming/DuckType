req "common" as common;

export mod {
  fn save() {
    common::exec(" 7");
  }

  fn restore() {
    common::exec("[u");
  }
}
