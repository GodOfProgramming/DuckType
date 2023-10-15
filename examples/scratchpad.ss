req "examples/ss_module/target/debug/ss_module";

use example_module.Foo;

let s = "wheee";
example_module.test_function(s);

let old = example_module.test_clear(s);
example_module.test_function(s);
example_module.test_function(old);

let f = Foo();
print(f.value);
f.value = 1;
print(f.value);
