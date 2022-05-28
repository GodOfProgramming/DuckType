fn fib(n) {
  if n <= 1 {
    n;
  } else {
    let a = fib(n - 2);
    let b = fib(n - 1);
  }
}

let times = 30;
print "times = " + times;
print fib(times);
