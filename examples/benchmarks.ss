req "time";

let REPS = 1000;

use time.Monotonic;

let x = 0;

let elapsed = 0;
for let i = 0; i < REPS; i += 1 {
  let before = Monotonic.now();
  x += i + i * i / i % i;
  elapsed += Monotonic.elapsed(before);
}

print "took " + (elapsed / REPS) + " seconds";
