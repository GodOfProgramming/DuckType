req "lib/sspec/sspec.ss" as sspec;
req "tests/util/cache_test/dependent_a.ss" as a_mod;
req "tests/util/cache_test/dependent_b.ss" as b_mod;

use sspec.describe;

describe("file caching", |t| {
  t.expect(a_mod.get_global()).to_be("B");
  t.expect(b_mod.get_global()).to_be("B");
});
