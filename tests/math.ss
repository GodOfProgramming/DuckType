req "sspec/sspec.ss" => sspec;

sspec.describe("math", (t) {
  let value = 1 + 2 * 3 - 4 / 5 + 6 * 7 - 8 * 9 / 10;
  let expected = 41;
  t.assert(value).to_be(expected);
});
