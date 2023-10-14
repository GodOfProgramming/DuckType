req "lib/sspec/sspec.ss";

use SSpec.describe;

use std.Object;

describe("empty structs", |t| {
  print t;
  let obj = struct {};
  print obj;
  let fields = Object.fields(obj);
  print fields;
  t.expect(fields.len()).to_be(0);
});

describe("struct fields", |t| {
  let num = 1;
  let obj = struct { num, str: "some string" };
  obj.foo = "bar";
  let fields = Object.fields(obj);
  t.expect(fields.len()).to_be(3);
  t.expect(obj.num).to_be(1);
  t.expect(obj.str).to_be("some string");
  t.expect(obj.foo).to_be("bar");

  let foo = struct {
    this: "this"
  };

  let bar = struct {
    that: "that",
  };

  let foobar = struct { foo, bar, baz: struct {  foobarbaz: "foobarbaz" } };

  t.expect(foo.this).to_be("this");
  t.expect(bar.that).to_be("that");
  t.expect(foobar.foo.this).to_be(foo.this);
  t.expect(foobar.bar.that).to_be(bar.that);
  t.expect(foobar.baz.foobarbaz).to_be("foobarbaz");
});
