fn generator() {
  let x = 0;
  let closure = {x} (offset) {
    x = x + offset;
    print x;
  };

  ret closure;
}

let gen = generator();
gen(1);
gen(2);
gen(3);
