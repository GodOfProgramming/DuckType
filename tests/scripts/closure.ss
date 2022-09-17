req "lib/sspec/sspec.ss";

class Number {
  new(self, n) {
    self.n = n;
  }

  fn add_and_store(self, n) {
    ret self.n += n;
  }
}

fn generator() {
  ret {x: Number(0)} |offset| {
    ret x.add_and_store(offset + 1);
  };
}

describe("captured values", |t| {
  let gen = generator();
  t.expect(gen(0)).to_be(1);
  t.expect(gen(1)).to_be(3);
  t.expect(gen(2)).to_be(6);
});
