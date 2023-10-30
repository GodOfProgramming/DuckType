let sspec = req "lib/sspec/sspec.ss";

use sspec::describe;

let $COUNTER = 1;

class SomeClass {
  fn second(self, t) {
    println "second";
    println self;
    println t;
    t.expect($COUNTER).to_be(2);
    $COUNTER += 1;
    ret self;
  }

  fn fourth(self, t) {
    println "fourth";
    println self;
    println t;
    t.expect($COUNTER).to_be(4);
  }
}

fn first(t) {
  println "first";
  println t;
  t.expect($COUNTER).to_be(1);
  $COUNTER += 1;
  ret t;
}

fn third(t) {
  println "third";
  println t;
  t.expect($COUNTER).to_be(3);
  $COUNTER += 1;
  ret t;
}

describe("ordering of functions", |t| {
  let instance = SomeClass();
  instance.second(first(t)).fourth(third(t));
  t.expect($COUNTER).to_be(4);
});
