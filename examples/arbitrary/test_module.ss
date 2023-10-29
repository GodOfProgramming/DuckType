class TestClass {}

class A {
  fn new_b() {
    ret B();
  }

  fn foo(self) {
    println("foo");
  }
}

class B {
  fn new_a() {
    ret A();
  }

  fn bar(self) {
    println("bar");
  }
}

fn some_func() {
  println("some func");
}

let x = "super secret string";

export mod {
  ClassA: A, ClassB: B,

  TestClass, RenamedClass: TestClass,

  some_func, bound_string: x,

  fn another_fn() {
    println("another func");
  }

  fn some_another_fn() {
    some_func();
  }

  class Foo {}

  mod Bar {
    mod Baz {}
  }
}
