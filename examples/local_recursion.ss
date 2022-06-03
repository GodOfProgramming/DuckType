fn ten_or_greater(arg) {
  if arg >= 10 {
    ret arg;
  }
  ret ten_or_greater(arg + 1);
}

print ten_or_greater(5);
