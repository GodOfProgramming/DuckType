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

export mod {
  class ModuleClass {
    new(self) {
      self.value = "MODULE";
    }

    fn call_global(self) {
      self.value = "GLOBAL";
      ret global_fn(self);
    }
  }
}

fn global_fn(x) {
  ret x.value;
}
