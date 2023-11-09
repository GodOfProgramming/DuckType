fn global_fn(x) {
  ret x.value;
}

export mod util {
  class ModuleClass {
    self as struct {
      value: nil
    }

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
