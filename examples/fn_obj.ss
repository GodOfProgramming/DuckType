fn function(arg) {
  print arg;
}

let value = "hello world";

let obj = { function, value };

obj.function(obj.value);
