class Number {
  new(self, n) {
    self <- n;
  }

  fn add_and_store(n) {
    self <- @self + n;
    ret @self;
  }
}

fn generator() {
  let x = Number(0);
  let closure = {x} |offset| {
    ret x.add_and_store(offset);
  };

  ret closure;
}

let gen = generator();
print gen(1);
print gen(2);
print gen(3);

({gen}||{
  print gen(10);
})();

let x = {}||{
  print "hello";
};

x();

let y = || {
  print "world";
};

let z = |arg| {
  print arg;
};

z("woo");

let abc = |arg1, arg2| {
  print arg1 + arg2;
};

abc("foo", "bar");

{}||{}();
