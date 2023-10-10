class Foo {
  new(self) {
    self.value = nil;
  }

  fn foo(self, value) {
    self.value = value;
    ret Bar(self);
  }
}

class Bar {
  new(self, foo) {
    self.foo = foo;
  }

  fn bar(self, value) {
    ret self.foo.value == value;
  }
}

fn truth() {
  ret true;
}

fn falsh() {
  ret false;
}

let f = Foo();

f.foo(truth()).bar(falsh());
