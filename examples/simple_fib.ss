fn fib(n) {
  if n <= 1 {
    ret n;
  } else {
    let a = fib(n - 2);
    let b = fib(n - 1);
    ret a + b;
  }
}

let times = 30;
print "times = " + times;
let before = clock();
print fib(times);
let after = clock();

print "diff = " + clock_diff(before, after);