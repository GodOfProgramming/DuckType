mod foo {
  BAR: 1,

  fn get_bar() {
    ret BAR;
  }
}

println(foo::get_bar());
