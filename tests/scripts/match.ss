req "./../../lib/sspec/sspec" as sspec;

use sspec.describe;

describe("random match", |t| {
  let choices = [1, 2, 3, 4];
  let random_value = choices.random_index();

  match random_value {
    1 => t.expect(random_value).to_be(1),
    2 => t.expect(random_value).to_be(2),
    3 => t.expect(random_value).to_be(3),
    4 => t.expect(random_value).to_be(4),
  } else {
    t.fail("match did not evaluate to a choice");
  }
});

describe("fixed match", |t| {
  let val = "match";
  match val {
    1 => t.fail("shouldn't match 1"),
    2 => t.fail("shouldn't match 2"),
    "match" => t.expect(val).to_be("match"),
  }

  let x = 0;

  match x {
    0 => x = 1,
  }

  t.expect(x).to_be(1);

  match x {
    2 => x = 0,
  } else {
    x = 100;
  }

  t.expect(x).to_be(100);
});
