req "ptr" => $ptr;
req "test/test.ss" => test;

print $LIBRARY;

print test;

class Example {
  new(word) {
    ret {word};
  }

  fn output() {
    print self.word;
  }
}

let example = Example("hello world");

example.output();

class Number {
  new(n) {
    ret n;
  }

  fn is_odd() {
    ret self.deref() % 2 != 0;
  }

  fn is_even() {
    ret self.deref() % 2 == 0;
  }

  fn deref() {
    ret $ptr.deref(self);
  }
}

let odd = Number(1);
let even = Number(2);

print "odd is odd: " + odd.is_odd();
print "even is even: " + odd.is_odd();
