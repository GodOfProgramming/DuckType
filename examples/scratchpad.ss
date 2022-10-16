req "lib/sspec/sspec.ss";

class Test {
  new(self, thing) {
    self.thing = thing;
  }

  fn do_thing(self) {
    ret self.thing;
  }
}

describe("test", |t| {
  let t = Test(1);
  let t2 = Test(t);
  print t2.do_thing().do_thing();

  let foo = [1, 2, 3];
  print foo[0];

  foo.push(4);
  print foo;
  print "methods: " + Test.methods;
  print "statics: " + Test.statics;
  # t.expect("foo").to_be("foo");
});
