class Test {
  new(self, thing) {
    self.thing = thing;
  }

  fn do_thing(self) {
    ret self.thing;
  }
}

let t = Test(1);
let t2 = Test(t);
print t2.do_thing().do_thing();
