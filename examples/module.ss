class TestClass {}

class A {
  fn new_b() {
    ret B();
  }

  fn foo(self) {
    print("foo");
  }
}

class B {
  fn new_a() {
    ret A();
  }

  fn bar(self) {
    print("bar");
  }
}

fn some_func() {
  print("some func");
}

let x = "super secret string";

export mod {
  ClassA: A;
  ClassB: B;

  TestClass;

  RenamedClass: TestClass;

  some_func;

  bound_string: x;

  fn another_func() {
    print("another func");
  }

  class Foo {}

  mod Bar {
    mod Baz {}
  }
}
