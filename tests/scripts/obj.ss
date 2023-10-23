let sspec = req "lib/sspec/sspec.ss";

use sspec::describe;
use std::obj;

describe("empty structs", |t| {
  let thing = struct {};
  let fields = obj::fields(thing);
  t.expect(fields.len()).to_be(0);
});

describe("struct fields", |t| {
  let num = 1;
  let thing = struct { num, str: "some string" };
  thing.foo = "bar";
  let fields = obj::fields(thing);
  t.expect(fields.len()).to_be(3);
  t.expect(thing.num).to_be(1);
  t.expect(thing.str).to_be("some string");
  t.expect(thing.foo).to_be("bar");

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
