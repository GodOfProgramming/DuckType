req "lib/sspec/bench" as bench;

use bench::benchmark;

let REPS = 10_000_000;

let x = 0;

benchmark("simple math", REPS, [x] |i| {
  i += 1;
  x += i + i * i / i % i;
});

fn simple_function() {}

benchmark("function calls", REPS, |_i| {
  simple_function();
});