class Test {
  fn test(self, arg) {
    self.x = arg;
  }

  fn exam(self) {
    print("test");
  }
}

class Other {
  fn @new() {
    let instance = Other();
    instance.x = 100;
    return instance;
  }

  fn test(this, arg) {
    this.y = arg;
  }
}

fn modify_test(t, arg) {
  t.test(arg);
}

let t1 = Test();
let t2 = Other();

modify_test(t1, 1);
modify_test(t2, 2);

print t1.x;
print t2.y;

let other = Other.new();
print other.x;

let other1 = other.new();
print other1.x;

print other == other1;
print other == t1;
print other == t2;

Test.exam(1);
