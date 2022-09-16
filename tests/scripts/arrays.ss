req "std";
req "lib/sspec/sspec.ss";

use std.Array;

describe("arrays.size_0", |t| {
  let arr = [];
  t.expect(Array.len(arr)).to_be(0);
  t.expect(arr[0]).to_be(nil);
});

describe("arrays.size_x", |t| {
  let arr = [1, 2, 3];
  t.expect(Array.len(arr)).to_be(3);

  for let i = 0; i < Array.len(arr); i += 1 {
    let ind = arr[i];
    t.expect(ind).to_be(i + 1);
  }
});
