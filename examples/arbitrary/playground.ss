mod Bar {
  class Baz {
    fn make_new(self) {
      ret Baz();
    }
  }
}

class Foo {
  fn make_new(self) {
    ret Foo();
  }
}

let foo = Foo();
println foo.make_new();

let baz = Bar::Baz();
println baz.make_new();

class Callable {
  new(self, x) {
    self.x = x;
  }

  fn __ivk__(self, param) {
    println(self + param);
  }

  fn __add__(self, other) {
    ret self.x + other;
  }
}

let c = Callable(1);

c(2);
