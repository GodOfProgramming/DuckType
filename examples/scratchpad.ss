mod Foo {
  class Bar {
    new(self, value) {
      self.value = value;
    }

    fn display(self) {
      print(self.value);
    }
  }
}

let bar = Foo.Bar("foobar");
bar.display();
