req "lib/sspec/bench";

let REPS = 1000000;

benchmark("simple math", REPS, {x: 0} |i| {
  i += 1;
  x += i + i * i / i % i;
});

fn simple_function() {}

benchmark("function calls", REPS, |_i| {
  simple_function();
});