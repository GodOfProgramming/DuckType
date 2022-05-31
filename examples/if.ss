let x = true;

if x {
  x = 1;
}

print x;

if x == 0 {
  print "bad";
} else {
  print "good once";
}

if !x {
  print "bad";
} else if x {
  print "good twice";
}
