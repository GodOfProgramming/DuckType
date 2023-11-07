fn foo(a, b, c) {
  ret a + b + c;
}

# fn bar(a, b, c) {
#   if a {
#     let d = 1;
#     ret d;
#   }
# }

# let x = 4;
# let y = 5;
# let baz = [x,y]|a,b,c|{ret a + b + c + x + y;};

println foo(1, 2, 3);
# println bar(1, 2, 3);
# println baz(1, 2, 3);