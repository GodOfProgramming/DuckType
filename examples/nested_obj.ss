let foo = {
  this: "this"
};

let bar = {
  that: "that",
};

let foobar = { foo, bar, baz: {  foobarbaz: "foobarbaz" } };

print "foobar = " + foobar;
