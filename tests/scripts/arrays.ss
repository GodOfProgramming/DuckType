console.flushln("start");
req "lib/sspec/sspec.ss";
console.flushln("required");

use std.Array;

console.flushln("describe1");
describe("arrays.size_0", |t| {
  console.flushln("in describe1");
  let arr = [];
  console.flushln("made array");
  t.expect(Array.len(arr)).to_be(0);
  console.flushln("expected");
  t.expect(arr[0]).to_be(nil);
  console.flushln("done");
});

console.flushln("describe2");
describe("arrays.size_x", |t| {
  let arr = [1, 2, 3];
  t.expect(Array.len(arr)).to_be(3);

  for let i = 0; i < Array.len(arr); i += 1 {
    let ind = arr[i];
    t.expect(ind).to_be(i + 1);
  }
});
