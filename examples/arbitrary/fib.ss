fn fib(n) {
  if n <= 1 {
    n;
  } else {
    fib(n - 2) + fib(n - 1);
  }
}

fn new_fib() {
  let c = |c, n| {
    if n <= 1 {
      n;
    } else {
      c(n - 2) + c(n - 1);
    }
  };

  c;
}

let begin = clock_seconds();
for let i = 0; i < 20; i = i + 1 {
  print fib(i);
}
let end = clock_seconds();

let fib_time = end - begin;
print "fib 0..20 took " + fib_time + " seconds";

let f = new_fib();

begin = clock_seconds();
for let i = 0; i < 20; i = i + 1 {
  print f(f, i);
}
end = clock_seconds();

let new_fib_time = end - begin;
print "new_fib 0..20 took " + new_fib_time + " seconds";

if fib_time > new_fib_time {
  print "closure is faster than functions";
} else if fib_time < new_fib_time {
  print "functions are faster than closures";
} else {
  print "they performed the same";
}
