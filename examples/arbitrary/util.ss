fn global_fn(x) {
  ret x.value;
}

export mod util {
  class ModuleClass {
    new(self) {
      self.value = "MODULE";
    }

    fn call_global(self) {
      self.value = "GLOBAL";
      ret global_fn(self);
    }

    fn clone(self) {
      ret self.__class__(self.value);
    }
  }
}
