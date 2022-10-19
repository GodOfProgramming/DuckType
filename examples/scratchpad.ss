class Test {
  new(self) {
    self._thing = 1;
  }

  fn +(self, other) {
    ret self._thing + other;
  }
}

print Test() + 1;
