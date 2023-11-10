req "option" as Option;

use std::console;

let $VERBOSE = Option(false);

let $FAIL_BY_RUNTIME_ERROR = Option(false);

let $PASS = ".";
let $FAILURE = "X";

class SpecTest {
  self as struct {
    failures: [],
  }

  fn expect(self, obj) {
    ret SpecExpect(obj, self.failures);
  }

  fn fail(self, msg) {
    self.failures.push(msg);
  }
}

class SpecExpect {
  self as struct {
    expected: nil,
    failures: nil,
  }

  new(self, expected, failures) {
    self.expected = expected;
    self.failures = failures;
  }

  fn to_be(self, actual) {
    if self.expected == actual {
      console::write($PASS);
    } else {
      if $VERBOSE.value {
        self.failures.push("expected " + std::debug(self.expected) + " to be " + std::debug(actual));
      } else {
        self.failures.push("expected " + self.expected + " to be " + actual);
      }
      console::write($FAILURE);
    }
  }

  fn to_be_truthy(self) {
    if self.expected {
      console::write($PASS);
    } else {
      self.failures.push("expected " + expected + " to be truthy");
      console::write($FAILURE);
    }
  }

  fn to_be_falsy(self) {
    if !self.expected {
      console::write($PASS);
    } else {
      self.failures.push("expected " + expected + " to be falsy");
      console::write($FAILURE);
    }
  }
}

export mod sspec {
  verbose: $VERBOSE,
  fail_by_runtime_error: $FAIL_BY_RUNTIME_ERROR,

  fn describe(descriptor, func) {
    let t = SpecTest();
    if func {
      func(t);

      console::writeln();

      if t.failures.len() == 0 {
        println "PASSED: " + descriptor;
        ret true;
      } else {
        println "FAILED: " + descriptor;
        for let i = 0; i < t.failures.len(); i += 1 {
          println t.failures[i];
        }
        if $FAIL_BY_RUNTIME_ERROR.value {
          undefined;
        } else {
          std::ps::exit(1); # TODO have suites return an error on failure
        }
        ret false;
      }
    } else {
      println "SKIPPED: "  + descriptor;
    }
  }
}
