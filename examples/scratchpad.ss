# fn foo(obj, n) {
#   ret obj.x += n;
# }
#
# let obj = {x:0};
# print foo(obj, 1);

let foo = {bar:{baz:0}};
let x;
let y;
x = y = foo.bar.baz = 5;
