fn foobar() {
  fn barfoo() {
    print "in barfoo";
  }

  barfoo();

  barfoo;
}

var x = foobar();

x();

foobar()();

|| {
  print "hello";
}();

var closure = |a| {
  print a;
};

closure(123);

var closure_result = |a, b| {
  a + b;
}(1, 2);

print closure_result;

print || { print "foo"; };

closure_result = |a| {
  if a {
    return 1;
  } else {
    return 0;
  }
}(true);

print closure_result;

fn counter() {
  var i = 0;
  || {
    i = i + 1;
    i;
  };
}

fn use_counter() {
  var c = counter();

  print c();
  print c();
  print c();
}

use_counter();
use_counter();

fn exec(f) {
  f();
}

exec(|| { print "executed"; });