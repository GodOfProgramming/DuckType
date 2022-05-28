fn first(arg1, arg2) {
  let x = 2;
  print x;
  fn second(arg) {
    print arg;
  }
  x = 1;
  print x;
  second(arg1 + arg2);
}

first(1, 2);
