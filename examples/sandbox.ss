class Example {
  new() {
    self <- 1;
    print self;
  }

  fn output() {
    print @self;
  }
}

let example = Example();
example.output();