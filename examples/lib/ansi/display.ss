req "common" as common;

export mod {
  fn clear() {
    common::exec("[J");
  }
}