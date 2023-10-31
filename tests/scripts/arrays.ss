let sspec = req "lib/sspec/sspec.ss";

use sspec::describe;

sspec::fail_by_runtime_error.set(true);

describe("arrays.size_0", |t| {
  let arr = [];
  t.expect(arr.len()).to_be(0);
  t.expect(arr[0]).to_be(nil);
});

describe("arrays.size_x", |t| {
  let arr = [1, 2, 3];
  t.expect(arr.len()).to_be(3);

  for let i = 0; i < arr.len(); i += 1 {
    let ind = arr[i];
    t.expect(ind).to_be(i + 1);
  }
});

describe("array index assign ops", |t| {
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;

    let x = a += b = c -= d;

    let va = [1.0; 1];
    let vb = [2.0; 1];
    let vc = [3.0; 1];
    let vd = [4.0; 1];

    let vx = va[0] += vb[0] = vc[0] -= vd[0];

    t.expect(a).to_be(va[0]);
    t.expect(b).to_be(vb[0]);
    t.expect(c).to_be(vc[0]);
    t.expect(d).to_be(vd[0]);
    t.expect(x).to_be(vx);
});
