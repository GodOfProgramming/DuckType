let util = req "util";

let g = GlobalClass("global");
let m = global_module.ModuleClass("module");

print(g.value);
print(m.value);
print(util);
