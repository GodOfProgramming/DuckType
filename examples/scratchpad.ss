class Test {
  new(self, thing) {
    self.thing = thing;
  }

  fn method(self) {
    ret self.thing;
  }
}

let t = Test(1);
let t2 = Test(t);
print t2.method().method();
