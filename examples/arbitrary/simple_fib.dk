use env.ARGV;

let times_str = ARGV[0];

if !times_str {
  println "forgot to give argument";
  ps.exit(1);
}

fn fib(n) {
  if n <= 1 {
    ret n;
  } else {
    let a = fib(n - 2);
    let b = fib(n - 1);
    ret a + b;
  }
}

fn fib_it(count) {
  let prev = 0;
  let curr = 1;

  for let i = 0; i < count; i = i + 1 {
    let tmp = curr;
    curr = curr + prev;
    prev = tmp;
  }

  ret prev;
}

let times = str.parse_number(times_str);
println "times = " + times;
let before = time.clock();
println fib(times);
let after = time.clock();

println "rec diff = " + time.clock_diff(before, after);

before = time.clock();
println fib_it(times);
after = time.clock();

println "it diff = " + time.clock_diff(before, after);
