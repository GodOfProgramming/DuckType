req "lib/sspec/sspec.ss";

describe("if branches", |t| {
  let v = true;

  if v {
    t.expect(v).to_be(true);
  } else {
    t.fail("if branched to wrong path");
  }

  if !v {
    t.fail("if branched to wrong path");
  } else {
    t.expect(v).to_be(true);
  }
});

describe("while loops", |t| {
  let n = 5;
  let i = 0;
  while i < n {
    i += 1;
  }

  t.expect(i).to_be(n);
});

describe("for loops", |t| {
  let n = 5;
  let i = 0;
  let total = 0;

  for let i = 0; i < n; i += 1 {
    total += i;
  }

  t.expect(i).to_be(0);
  t.expect(total).to_be(10);
});

describe("matches", |t| {
  let val = "match";
  match val {
    1 -> t.fail("shouldn't match 1"),
    2 -> t.fail("shouldn't match 2"),
    "match" -> t.expect(val).to_be("match"),
  }

  let x = 0;

  match x {
    0 -> x = 1,
  }

  t.expect(x).to_be(1);

  match x {
    2 -> x = 0,
  } else {
    x = 100;
  }

  t.expect(x).to_be(100);
});

describe("loops", |t| {
  let i = 0;
  let n = 5;
  loop {
    i += 1;
    if i == n {
      break;
    }
  }

  t.expect(i).to_be(n);
});

fn recursive_fn(i) {
  if i >= 5 {
    ret i;
  }
  ret recursive_fn(i + 1);
}

describe("recursion", |t| {
  t.expect(recursive_fn(0)).to_be(5);
  t.expect(recursive_fn(10)).to_be(10);
});

describe("ands ors", |t| {
  t.expect(true and false or true).to_be(true);
  t.expect(true and false and false or true).to_be(true);
  t.expect(true and false and (false or true)).to_be(false);
  t.expect(true and false).to_be(false);
  t.expect(true and (true and (false or false))).to_be(false);
});
