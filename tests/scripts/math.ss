req "lib/sspec/sspec.ss";

describe("pemdas", |t| {
  let value = (1 + 2) * 3 - 4 / 5 + 6 * 7 - 8 * 9 / 10;
  let expected = 43;
  t.expect(value).to_be(expected);
});
