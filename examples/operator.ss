class Add {
  new(self, value) {
    self.value = value;
  }

  fn __add__(self, other) {
    ret self.value + other;
  }
}

print Add(1) + 2;