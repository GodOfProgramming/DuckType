req "lib/sspec/bench" as bench;

use bench::benchmark;

let REPS = 10_000_000;

let x = 0;

benchmark("DISABLED_simple math", REPS, [x] |timer: Timer, i| {
  i += 1;
  x += i + i * i / i % i;
});

fn simple_function() {}

benchmark("DISABLED_function calls", REPS, |timer: Timer, _i| {
  simple_function();
});

let $OBJ = struct {
  foobarbaz: "foobarbaz"
};

# global & member access took 9.154016394 seconds
benchmark("global & member access", REPS, |timer: Timer, _| {
  timer.start();
  $OBJ.foobarbaz;
  timer.stop();
});