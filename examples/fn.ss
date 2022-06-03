fn first(arg1, arg2) {
  second(arg1 + arg2);
}

fn second(arg) {
  print arg;
}


first(1, 2);
