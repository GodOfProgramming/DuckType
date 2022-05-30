fn first(arg1, arg2) {
  fn second(arg) {
    print arg;
  }
  second(arg1 + arg2);
}

print first(1, 2);
