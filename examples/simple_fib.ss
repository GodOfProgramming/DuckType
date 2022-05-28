fn fib(n, cnt) {
  print "cnt is " + cnt;
  if n <= 1 {
    n;
  } else {
    cnt = cnt + 1;
    fib(n - 2, cnt);

    cnt = cnt + 1;
    fib(n - 1, cnt);
  }
}

print "5";
let x = 5;
print fib(x, 0);
print "x is " + x;
# print "20";
# print fib(20, 0);
# print "50";
# print fib(50, 0);
# print "100";
# print fib(100, 0);