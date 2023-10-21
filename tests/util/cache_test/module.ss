let $GLOBAL = nil;

fn set_a() {
  $GLOBAL = "A";
}

fn set_b() {
  $GLOBAL = "B";
}

fn get_global() {
  ret $GLOBAL;
}

export mod { set_a, set_b, get_global }
