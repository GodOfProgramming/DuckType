class GlobalClass {
  new(self, value) {
    self.value = value;
  }
}

mod global_module {
  class ModuleClass {
    new(self, value) {
      self.value = value;
    }
  }
}

ret "yay";
