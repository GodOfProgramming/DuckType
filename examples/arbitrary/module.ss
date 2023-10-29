let $GLOBAL = nil;

fn foo() {
  $GLOBAL = "foo";
}

fn bar() {
  $GLOBAL = "bar";
}

fn print_global() {
  println($GLOBAL);
}

export mod { foo, bar, print_global }