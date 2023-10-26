mod Bar {
  class Baz {
    fn make_new(self) {
      ret Bar::Baz();
    }
  }
}

class Foo {
  fn make_new(self) {
    ret Foo();
  }
}

let foo = Foo();
print foo.make_new();

let baz = Bar::Baz();
print baz.make_new();