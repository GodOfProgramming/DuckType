let $GLOBAL = true;
fn impossible_condition() {
  ret $GLOBAL;
}

if impossible_condition() {
  __breakpoint__;
}

if impossible_condition() {
  print($oh_no_an_undefined_variable);
}
