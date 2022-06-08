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
  new(n) { self <- n; }

  fn increment() {
    self <- @self + 1;
  }

  fn is_odd() {
    ret @self % 2 != 0;
  }

  fn is_even() {
    ret @self % 2 == 0;
  }
}

let odd = Number(1);
let even = Number(2);

print @odd + " is odd: " + odd.is_odd();
print @even + " is even: " + even.is_even();

odd.increment();
even.increment();

print @odd + " is odd: " + odd.is_odd();
print @even + " is even: " + even.is_even();
