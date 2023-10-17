let module = req "examples/module";

print(std.debug(module));

use module.ClassA;
use module.ClassB;

let b = ClassA.new_b();
let a = ClassB.new_a();

a.foo();
b.bar();
