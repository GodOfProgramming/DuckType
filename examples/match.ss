let value = "match";

match value {
  1 => print "found 1";
  2 => print "foo";
  "match" => print "found 'match'";
  => print "found other";
}