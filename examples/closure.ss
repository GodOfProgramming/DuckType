req "console" => $console;

fn generator() {
  let x = 0;
  let closure = {x} (offset) {
    x = x + offset;
    ret x;
  };

  ret closure;
}

let gen = generator();
print gen(1);
print gen(2);
print gen(3);

({gen}(){
  print gen(10);
})();

let x = {}() {
  print "hello";
};

x();

let y = () {
  print "world";
};

let z = (arg) {
  print arg;
};

z("woo");

let abc = (arg1, arg2) {
  print arg1 + arg2;
};

abc("foo", "bar");
