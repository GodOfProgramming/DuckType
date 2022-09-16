req "std";
req "lib/sspec/sspec.ss";

let arr = [1, 2, 3];
print std.Array.len(arr);

describe("simple", |t| {
  t.expect(1).to_be(1);
  t.expect(1).to_be(2);
});
