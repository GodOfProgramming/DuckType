fn first(arg1, arg2) {
  fn second(arg) {
    print arg;
  }
  second(arg1 + arg2);
}

first(1, 2);
