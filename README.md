# SimpleScript

## Introduction

First and foremost, this language was not designed to be used standalone, it was meant to be a language that works alongside a Rust program by embedding it. You still can and it'll work, but if you run into problems like "why can't I load a native library", that's why.

It's syntax is similar to Rust in many ways, but naturally has some differences, especially in the safety department. Being a scripting language there are almost no type safety checks.

### Classes

Classes function much like other familiar languages. Declare the class with the `class` keyword and proceed to fill in the body with methods you'd like to associate with it. The notable difference however is there are no special constructor methods. You instantiate a class by calling the class name much like a regular function. However if constructor like methods are desired you can create static methods that are bound to the class. All static methods are prefixed with an '@'. See following example for clarity.

[//]: # "popular languages with known syntax have an advantage here"

```js
class Demo {
```

```rust
  new(offset) {
    print "This is a constructor";
    ret {offset};
  }

  fn method(param1, param2) {
    self.x = self.offset + param1;
    self.y = self.offset + param2;
  }
```

```js
}


let demo = Demo(123);
demo.method(45, 67);
```

```perl
print demo; # <instance of Demo> Struct { offset: 123, x: 168, y: 190 }
```
