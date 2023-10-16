let sspec = req "lib/sspec/sspec.ss";
let util = req "util";

let instance = util.ModuleClass("module");

print(instance.value);
print(std.reflect.defined("global_fn"));
print(instance.call_global());
print(instance.clone());
