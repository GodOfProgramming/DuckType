class Example {
  new(option) {
    if option {
      self <- "example of true";
    } else {
      self <- "example of false";
    }
  }

  fn output() {
    print @self;
  }
}

class ExampleBuilder {
  new() {
    self <- {
      option: false,
    };
  }

  fn build() {
    ret Example(self.option);
  }

  fn enable_option() {
    self.option = false;
    ret self;
  }
}

let builder = ExampleBuilder();

let example = builder.enable_option().build();

example.output();
