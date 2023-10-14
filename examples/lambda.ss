let arr = [1, 2, 3];
print(arr);

let obj = struct {
  value: "foobar"
};

print(obj.value);

let lambda = [obj] |value| {
  obj.value = value;
};

lambda("barfoo");

print(obj.value);

let ass_lambda = [obj] |value| {
  obj.value = value;
};

ass_lambda(4);

print(arr);

print(obj);
