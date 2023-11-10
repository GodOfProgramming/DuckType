req "timer" as Timer;

use std::str;
use std::time::mono;

export mod bench {
  fn benchmark(descriptor, reps, func) {
    if !str::is_prefix(descriptor, "DISABLED") {
      println "running " + descriptor;

      let timer = Timer();

      for let i = 0; i < reps; i += 1 {
        func(timer, i);
      }

      let elapsed = timer.elapsed;

      println(str::concat(descriptor, " took ", elapsed, " seconds", ", or ", elapsed / reps, " per exec"));
    }
  }
}
