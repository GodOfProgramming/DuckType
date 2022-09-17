req "lib/sspec/bench";

let REPS = 1000000;

benchmark("DISABLED simple math", REPS, {x: 0} |i| {
  x += i + i * i / i % i;
});

fn simple_function() {}

benchmark("function calls", REPS, |_i| {
  simple_function();
});
