fn impossible_condition() {
  ret true;
}

if impossible_condition() {
  __breakpoint__;
}
