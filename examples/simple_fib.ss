fn fib(n) {
  if n <= 1 {
    n;
  } else {
    fib(n - 2) + fib(n - 1);
  }
}

print fib(5);
print fib(20);
print fib(50);