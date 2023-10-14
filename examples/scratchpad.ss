let Foo = class {
  new(self, value) {
    self.value = value;
  }

  fn display(self) {
    print(self.value);
  }
};

let foo = Foo("foo");
foo.display();
