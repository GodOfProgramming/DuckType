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

  let foo = {
    this: "this"
  };

  let bar = {
    that: "that",
  };

  let foobar = { foo, bar, baz: {  foobarbaz: "foobarbaz" } };

  t.expect(foo.this).to_be("this");
  t.expect(bar.that).to_be("that");
  t.expect(foobar.foo.this).to_be(foo.this);
  t.expect(foobar.bar.that).to_be(bar.that);
  t.expect(foobar.baz.foobarbaz).to_be("foobarbaz");
});
