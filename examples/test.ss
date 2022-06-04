req "console" => $console;

fn runner(desc, f) {
  $console.write("testing ", desc);
  let t = {
    expect: {} (arg) {
      ret {
        to_equal: {arg} (expected) {
          if arg == expected {
            $console.write(arg, " equaled ", expected);
          } else {
            $console.write(arg, " not equal to ", expected);
          }
        }
      };
    }
  };
  f(t);
}

let t = { describe: runner };

let obj = 1;
t.describe("object", {obj}(t) {
  t.expect(obj).to_equal(0);
});
