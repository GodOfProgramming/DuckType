class Foo {
  new(self, v) {
    self.value = v;
  }

  fn display(self) {
    println(self);
  }
}

let f = Foo(1);

f.display();
