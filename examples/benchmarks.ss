req "lib/clock.ss";

let REPS = 1000;

let x = 0;

let clock = Clock();
for let i = 1; i < REPS; i += 1 {
  clock.start();
  x += i + i * i / i % i;
  clock.stop();
}

print "took " + (clock.elapsed() / REPS) + " seconds";
