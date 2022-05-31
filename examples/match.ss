match "match" {
  1 => { print "bad 1"; }
  2 => { print "bad 2"; }
  "match" => { print "good"; }
}

let x = 0;

match x {
  0 => x = 1,
}

print x;

match x {
  2 => x = 0,
} else {
  x = 100;
}

print x;
