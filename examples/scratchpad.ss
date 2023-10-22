let test = req "examples/ss_module/target/debug/ss_module.dll";

use test.Foo;

let f = Foo(1);

print("foo is " + f);

req "examples/test_module" as module;

print(std.debug(module));

module.some_another_fn();

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
