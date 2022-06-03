fn generator() {
  let x = 0;
  let closure = {x} () {
    x = x + 1;
    print x;
  };

  ret closure;
}

let gen = generator();
gen();
gen();
gen();
