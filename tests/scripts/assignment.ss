req "lib/sspec/sspec.ss";

let $VALUE = "new value";

describe("chained assignment", |t| {
  let a;
  let b = nil;
  let c = b = a = $VALUE;
  t.expect(a).to_be($VALUE);
  t.expect(b).to_be($VALUE);
  t.expect(c).to_be($VALUE);

  {
    let local1;
    let local2;
    let local3;
    let local4;
    let local5;
    let local6;

    local1 = local2 = local3 = local4 = local5 = local6 = a;

    t.expect(local1).to_be($VALUE);
    t.expect(local2).to_be($VALUE);
    t.expect(local3).to_be($VALUE);
    t.expect(local4).to_be($VALUE);
    t.expect(local5).to_be($VALUE);
    t.expect(local6).to_be($VALUE);
  }
});
