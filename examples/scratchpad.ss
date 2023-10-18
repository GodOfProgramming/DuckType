let module = req "examples/module";

print(std.debug(module));

use module.ClassA;
use module.ClassB;

let b = ClassA.new_b();
let a = ClassB.new_a();

a.foo();
b.bar();

fn get_str() {
  ret "some string";
}

let x = get_str();
let y = get_str();
x.replace_with("foobar");
print(x);
print(y);
