let util = req "util";

let instance = util.ModuleClass("module");

print(instance.value);
print(global_fn);
print(instance.call_global());
print(instance.clone());
