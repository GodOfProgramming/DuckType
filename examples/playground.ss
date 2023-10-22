req "lib/sspec/sspec" as sspec;

use sspec.describe;

let $x = 0;

describe("simple math", |t| {
  let i = 1_000_000;
  i += 1;
  $x += i + i * i / i % i;
  print($x);
  t.expect($x > 0).to_be(true);
});