req "common" as common;

export mod display {
  fn clear() {
    common::exec("[J");
  }
}