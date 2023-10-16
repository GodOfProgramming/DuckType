let sspec = req "lib/sspec/sspec.ss";

use sspec.describe;

let $COUNTER = 1;

class SomeClass {
  fn second(self, t) {
    print "second";
    print self;
    print t;
    t.expect($COUNTER).to_be(2);
    $COUNTER += 1;
    ret self;
  }

  fn fourth(self, t) {
    print "fourth";
    print self;
    print t;
    t.expect($COUNTER).to_be(4);
  }
}

fn first(t) {
  print "first";
  print t;
  t.expect($COUNTER).to_be(1);
  $COUNTER += 1;
  ret t;
}

fn third(t) {
  print "third";
  print t;
  t.expect($COUNTER).to_be(3);
  $COUNTER += 1;
  ret t;
}

describe("ordering of functions", |t| {
  let instance = SomeClass();
  instance.second(first(t)).fourth(third(t));
  t.expect($COUNTER).to_be(4);
});
