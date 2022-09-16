req "std";
req "ps";
req "console";

fn describe(descriptor, func) {
  let vec = std.Vec(1);
  let t = "xyz";
  func(t);

  console.writeln();

  if vec.len() == 0 {
    print "PASSED: " + descriptor;
  } else {
    print "FAILED: " + descriptor;
    for let i = 0; i < vec.len(); i += 1 {
      print vec[i];
    }
    ps.exit(1);
  }
}

describe("foobar", |t|{});
