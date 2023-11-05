class Foo {
  self as struct { x: "x" }

  fn display(self) {
    println self.x;
  }
}

let foo = Foo();
foo.display();
