req "env" => env;
req "time" => time;
req "string" => str;
req "console" => console;

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

let times = str.parse_number(env.ARGV[0]);
print "times = " + times;
let before = time.clock();
print fib(times);
let after = time.clock();

print "rec diff = " + time.clock_diff(before, after);

before = time.clock();
print fib_it(times);
after = time.clock();

print "it diff = " + time.clock_diff(before, after);