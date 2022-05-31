let var = "global";
{
  var = "global was inside local";
  let var = "local";
  print var;
}
print var;