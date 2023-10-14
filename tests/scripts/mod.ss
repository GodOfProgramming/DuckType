req "lib/sspec/sspec.ss";

use sspec.describe;

let $TEST_STRING = "foobarbaz";

mod Foo {
  class Bar {
    new(self, value) {
      self.value = value;
    }

    fn display_string(self) {
      ret self.value;
    }
  }

  mod Baz {
    fn foobarbaz() {
      ret $TEST_STRING;
    }
  }
}

describe("modules", |t| {
  let test_string = "foobar";
  let bar = Foo.Bar(test_string);
  t.expect(bar.display_string()).to_be(test_string);
  t.expect(Foo.Baz.foobarbaz()).to_be($TEST_STRING);
});