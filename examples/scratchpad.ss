class Foo {
  new(self) {
    self.value = nil;
  }

  fn foo(self, value) {
    print(2);
    self.value = value;
    ret Bar(self);
  }
}

class Bar {
  new(self, foo) {
    self.foo = foo;
  }

  fn bar(self, value) {
    print(4);
    ret self.foo.value == value;
  }
}

fn truth() {
  print(1);
  ret true;
}

fn falsh() {
  print(3);
  ret false;
}

let f = Foo();

print f.foo(truth()).bar(falsh());
