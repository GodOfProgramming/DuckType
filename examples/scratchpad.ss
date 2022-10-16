class Test {
  new(self) {
    self._thing = {};
  }

  fn thing(self) {
    ret self._thing;
  }
}

let t = Test();

t.thing().foo = "value";

print "new thing = " + Test().thing();
