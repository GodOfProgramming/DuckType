let sspec = req "lib/sspec/sspec.ss";

use sspec::describe;

describe("truthy values", |t| {
  t.expect(true).to_be_truthy();
  t.expect(!false).to_be_truthy();
  t.expect(!nil).to_be_truthy();
  t.expect(0).to_be_truthy();
  t.expect(1).to_be_truthy();
});

describe("falsy values", |t| {
  t.expect(false).to_be_falsy();
  t.expect(!true).to_be_falsy();
  t.expect(nil).to_be_falsy();
  t.expect(!0).to_be_falsy();
  t.expect(!1).to_be_falsy();
});
