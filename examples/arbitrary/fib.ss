let arr = [];

fn fib(n) {
  if n <= 1 {
    ret n;
  } else {
    ret fib(n - 2) + fib(n - 1);
  }
}

for let i = 0; i < 20; i = i + 1 {
  arr.push(fib(i));
}

println arr.join(", ");
