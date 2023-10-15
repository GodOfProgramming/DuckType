req "examples/ss_module/target/debug/ss_module";

let s = "wheee";
example_module.test_function(s);

let old = example_module.test_clear(s);
example_module.test_function(s);
example_module.test_function(old);
