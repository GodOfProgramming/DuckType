req "std";
req "lib/sspec/sspec.ss";

let obj = { num: 1, str: "some string" };
obj = {};
obj.foo = "bar";

describe("empty structs", |t| {
  let obj = {};
  let fields = std.Object.fields(obj);
  t.expect(std.Array.len(fields)).to_be(0);
});

describe("struct fields", |t| {
  let num = 1;
  let obj = { num, str: "some string" };
  obj.foo = "bar";
  let fields = std.Object.fields(obj);
  t.expect(std.Array.len(fields)).to_be(3);
  t.expect(obj.num).to_be(1);
  t.expect(obj.str).to_be("some string");
  t.expect(obj.foo).to_be("bar");
});
