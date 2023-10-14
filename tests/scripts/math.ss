req "lib/sspec/sspec.ss";

use SSpec.describe;

$SSPEC_VERBOSE = true;

describe("pemdas", |t| {
  let value = (1 + 2) * 3 - 4.0 / 5 + 6 * 7 - 8 * 9.0 / 10;
  let expected = 43;
  t.expect(value).to_be(expected);
});
