mod Foo {
  class Bar {
    new(self, value) {
      self.value = value;
    }

    fn display(self) {
      print(self.value);
    }
  }

  mod Baz {
    fn foobarbaz() {
      print("foobarbaz");
    }
  }
}

let bar = Foo.Bar("foobar");
bar.display();

Foo.Baz.foobarbaz();
