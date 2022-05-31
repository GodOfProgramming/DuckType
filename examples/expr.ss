let a;
let b = nil;
let c = b = a = "new value";
print a;
print b;
print c;

{
  let local1;
  let local2;
  let local3;
  let local4;
  let local5;
  let local6;

  local1 = local2 = local3 = local4 = local5 = local6 = a;

  print local1;
  print local2;
  print local3;
  print local4;
  print local5;
  print local6;
}