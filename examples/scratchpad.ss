let sspec = req "lib/sspec/sspec.ss";

use sspec.describe;

sspec.verbose.set(true);

describe("test verbose switch", |t| {
  t.expect(true).to_be(false);
});
