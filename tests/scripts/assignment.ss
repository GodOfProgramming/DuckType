req "lib/sspec/sspec.ss";

describe("chained assignment", |t| {
  let a;
  let b = nil;
  let c = b = a = "new value";
  t.expect(a).to_be("new value");
  t.expect(b).to_be("new value");
  t.expect(c).to_be("new value");

  {
    let local1;
    let local2;
    let local3;
    let local4;
    let local5;
    let local6;

    local1 = local2 = local3 = local4 = local5 = local6 = a;

    t.expect(local1).to_be("new value");
    t.expect(local2).to_be("new value");
    t.expect(local3).to_be("new value");
    t.expect(local4).to_be("new value");
    t.expect(local5).to_be("new value");
    t.expect(local6).to_be("new value");
  }
});
