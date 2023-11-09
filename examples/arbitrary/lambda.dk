let arr = [1, 2, 3];
println(arr);

let obj = struct {
  value: "foobar"
};

println(obj.value);

let lambda = [obj] |value| {
  obj.value = value;
};

lambda("barfoo");

println(obj.value);

let ass_lambda = [obj] |value| {
  obj.value = value;
};

ass_lambda(4);

println(arr);

println(obj);
