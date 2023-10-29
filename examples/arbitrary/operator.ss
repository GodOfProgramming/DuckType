class Add {
  new(self, value) {
    self.value = value;
  }

  fn __add__(self, other) {
    ret self.value + other;
  }
}

println Add(1) + 2;